let max_memory_read_file_size = 1 lsl 30
let max_io_chunk = 16 * 1024

type error_kind =
  | Too_Big_Read [@@deriving sexp]

exception IOErrorException of error_kind [@@deriving sexp]

exception UserCancelException

exception Exception_List of exn list [@@deriving sexp]

type tty_handle = Lwt_unix.file_descr

type 'a t = 'a Lwt.t

open Lwt.Infix

let run_io = Lwt_main.run

let return = Lwt.return
let return_unit = Lwt.return_unit
let return_false = Lwt.return_false
let return_true = Lwt.return_true
let return_none = Lwt.return_none
let return_some = Lwt.return_some
let bind = Lwt.bind

let user_cancel_flag = ref false

let str = Printf.sprintf

let combine_exceptions e1 e2 =
  if e1 = e2 then e1
  else
    match (e1, e2) with
    | (Lwt.Canceled, e) -> e
    | (e, Lwt.Canceled) -> e
    | _ ->
      (* TODO consider merging common exceptions from exception lists *)
      let list = match (e1, e2) with
        | (Exception_List list, Exception_List list2)
          -> (List.append list list2)
        | (Exception_List list, e) -> List.append list [e]
        | (e, Exception_List list) -> e :: list
        | (e1, e2) -> [e1; e2]
      in
      Exception_List list

let with_cancel action =
  let (cancel_notifier , _): ('a Lwt.t * 'a Lwt.u) = Lwt.task () in
  let check_cancel () =
    if Lwt.is_sleeping cancel_notifier then Lwt.return_unit
    else Lwt.fail Lwt.Canceled
  in
  let action_promise = action check_cancel in
  let selector = Lwt.pick [cancel_notifier; action_promise ] in
  Lwt_result.catch selector >>= fun selector_result ->
    (*
     * As pick cancels sleeping promises and the only way for
     * cancel_notifier to resolve is via cancel, it must be
     * canceled at this point.
     *)
    let () = match Lwt.state cancel_notifier with
      | Lwt.Fail Lwt.Canceled -> ()
      | _ -> assert false
    in
    match selector_result with
    | Result.Ok value -> return value
    | _ ->
      (*
       * The action failed or pick was canceled. In the latter case the
       * action may has ignored the cancellation and continued to sleep,
       * so ensure that we waits for the result after we canceled.
       *)
      action_promise >>= fun _ -> Lwt.fail Lwt.Canceled

let error_cleanup action =
  fun exn ->
    let on_finalize_error exn2 =
      Lwt.fail (combine_exceptions exn exn2)
    in
    Lwt.try_bind action (fun () -> Lwt.fail exn) on_finalize_error

let with_cleanup cleanup body =
  let on_body_result value = cleanup () >>= fun () -> return value in
  Lwt.try_bind body on_body_result (error_cleanup cleanup)

let use_or_cancel promise action =
  let close_promise () =
    (* Try to cancel promise and then wait for its result if any. *)
    let () = Lwt.cancel promise in
    Lwt.try_bind
      (fun () -> promise)
      (fun _ -> return_unit)
      (function | Lwt.Canceled -> return_unit | e -> Lwt.fail e)
  in
  with_cleanup close_promise action

let safe_join2 promise1 promise2 =
  Lwt.catch (fun () -> Lwt.join [promise1; promise2]) @@ fun _ ->

  (*
   * We do not know which promised failed. So ignore the catch result
   * and ask both for the result ensuring that we really waited until
   * both promises are resolved.
   *)
  Lwt_result.catch promise1 >>= fun result1 ->
  Lwt_result.catch promise2 >>= fun result2 ->
  match (result1, result2) with
  | (Result.Ok (), Result.Ok ()) -> assert false
  | (Result.Ok (), Result.Error exn) -> Lwt.fail exn
  | (Result.Error exn, Result.Ok ()) -> Lwt.fail exn
  | (Result.Error exn1, Result.Error exn2) ->
    Lwt.fail (combine_exceptions exn1 exn2)

let err text = Lwt.fail (Failure text)

let user_cancel () = Lwt.fail UserCancelException

(*
 * Convenient function to close the file descriptor after some action.
 *)
let with_fd_close fd action =
  with_cleanup (fun () -> Lwt_unix.close fd) action

let read_file_fully path =
  let read_flags = Lwt_unix.([ O_RDONLY; O_NONBLOCK; O_CLOEXEC ]) in
  Lwt_unix.openfile path read_flags 0 >>= fun fd ->
  with_fd_close fd @@ fun () ->
  Lwt_unix.LargeFile.fstat fd >>= fun stats ->
  let file_size = Lwt_unix.LargeFile.(stats.st_size) in
  if Int64.compare file_size (Int64.of_int max_memory_read_file_size) > 0
  then Lwt.fail (IOErrorException Too_Big_Read)
  else
    let n = Int64.to_int file_size in
    if n = 0 then return ""
    else
      let buffer = Bytes.create n in
      let offset = ref 0 in
      let rec read_chunk () =
        let chunk_size = min max_io_chunk (n - !offset) in
        Lwt_unix.read fd buffer !offset chunk_size >>= process_read_result
      and process_read_result nread =
        if nread = 0 then
          (* File was truncated while reading. *)
          return (Bytes.sub_string buffer 0 !offset)
        else
          let new_offset = !offset + nread in
          if new_offset = n then
            return (Bytes.unsafe_to_string buffer)
          else
            Lwt_unix.yield () >>= read_chunk
      in
      read_chunk ()

let read_descriptor_fully fd =
    (*
      Read in chunks of doubling size but no greater than
      {!max_io_chunk} and then copy all tread chunks to the final
      string.
    *)
    let module Data = struct
      type local = {
        (* current chunk to read *)
        mutable chunk : bytes;

        (* amount of read so far data in the chunk *)
        mutable size : int;

        (* collection of previously read chunks *)
        mutable prev_chunks : bytes list;

        (* size of all chunks in {!prev_chunks} *)
        mutable prev_total_size : int;
      }
    end in
    let open Data in
    let data = {
        prev_chunks = [];
        prev_total_size = 0;
        chunk = Bytes.create (min 128 max_io_chunk);
        size = 0;
    } in
    let rec read_chunk () =
        let remaining = Bytes.length data.chunk - data.size in
        Lwt_unix.read fd data.chunk data.size remaining >>= process_read_result
    and process_read_result nread =
      if nread != 0 then
        let size = nread + data.size in

        (* ok is false when we reach max_memory_read_file_size *)
        let ok : bool =
            let capacity = Bytes.length data.chunk in
            if size != capacity then
            begin
              data.size <- size;
              true
            end
            else
              let total_size = data.prev_total_size + size in
              let buffer_size_limit = max_memory_read_file_size - total_size in
              if buffer_size_limit = 0 then
                false
              else
                let new_capacity =
                    min buffer_size_limit (min max_io_chunk (2 * capacity))
                in
                begin
                  data.prev_chunks <- data.chunk :: data.prev_chunks;
                  data.prev_total_size <- total_size;
                  data.chunk <- Bytes.create new_capacity;
                  data.size <- 0;
                  true
                end
        in
        if not ok then
          Lwt.fail (IOErrorException Too_Big_Read)
        else
          Lwt_unix.yield () >>= read_chunk
      else
        let buffer = Bytes.create (data.prev_total_size + data.size) in
        begin
          (* copy from tail towards start of buffer as chunks follow LIFO order *)
          Bytes.blit data.chunk 0 buffer data.prev_total_size data.size;
          let copy_one end_offset chunk =
            let chunk_size = Bytes.length chunk in
            let offset = end_offset - chunk_size in
            begin
              Bytes.blit chunk 0 buffer offset chunk_size;
              offset
            end
          in
          let offset =
            List.fold_left copy_one data.prev_total_size data.prev_chunks
          in
          let () = assert (offset = 0) in
          return (Bytes.unsafe_to_string buffer)
        end
    in
    read_chunk ()


let write_fd fd text =
    let offset = ref 0 in
    let n = String.length text in
    let rec write_chunk () =
        let remaining = min max_io_chunk (n - !offset) in
        if remaining = 0 then
          return ()
        else
          Lwt_unix.write_string fd text !offset remaining >>= process_write_result
    and process_write_result nwritten =
        let () = offset := !offset + nwritten in
        Lwt_unix.yield () >>= write_chunk
      in
    write_chunk ()

let write_stdout = write_fd Lwt_unix.stdout

let write_stderr = write_fd Lwt_unix.stderr

let write_file path text =
    (*
     * Write using a temporary so if the process crashes or killed, old
     * file is preserved.
     *)
    let tmp_path = path ^ ".tmp" in
    let open_flags =
      Lwt_unix.([ O_CREAT; O_WRONLY; O_TRUNC; O_NONBLOCK; O_CLOEXEC ])
    in
    Lwt_unix.openfile tmp_path open_flags 0o666 >>= fun tmp_fd ->
    let write_rename () =
      write_fd tmp_fd text >>= fun () ->
      Lwt_unix.close tmp_fd >>= fun () ->
      Lwt_unix.rename tmp_path path
    in
    let on_rename_error () =
      (* close can fail with error from a previous write operation. *)
      with_cleanup (fun () -> Lwt_unix.unlink tmp_path) @@ fun () ->
      Lwt_unix.close tmp_fd
    in
    Lwt.catch write_rename (error_cleanup on_rename_error)

let lwt_bytes_to_string buffer offset length =
    let result = Bytes.create length in
    let () = Lwt_bytes.blit_to_bytes buffer offset result 0 length in
    Bytes.unsafe_to_string result

(**
 * utility to read a line from a tty terminal when the read does not
 * return anything extra after the new line. Return (full_line, str)
 * where full_line is true if the full line was read. false indicates
 * eol was hit before the end-of-line. str never includes the line
 * terminator.
 *)
let read_tty_line (fd: tty_handle): (bool * string) Lwt.t =
  let rec read_step buffer size =
    let remaining = Lwt_bytes.length buffer - size in
    let () = assert (remaining > 0) in
    Lwt_bytes.read fd buffer size remaining >>= fun nread ->
    if nread == 0 then
      return (false, lwt_bytes_to_string buffer 0 size)
    else
      let new_size = size + nread in
      let last = new_size - 1 in
      if '\n' = Lwt_bytes.get buffer last then
        (* return stripping new line *)
        return (true, lwt_bytes_to_string buffer 0 last)
      else
        let new_buffer =
          if new_size < Lwt_bytes.length buffer then buffer
          else
            let bigger = Lwt_bytes.create (new_size * 2) in
            let () = Lwt_bytes.blit buffer 0 bigger 0 new_size in
            bigger
        in
        read_step new_buffer new_size
  in
  read_step (Lwt_bytes.create 128) 0

let with_tty action =
  let do_open () =
    Lwt_unix.(openfile "/dev/tty" [O_NONBLOCK; O_CLOEXEC; O_RDWR] 0)
  in
  let process_tty tty_fd =
    with_fd_close tty_fd (fun () -> action tty_fd) >>= fun value ->
    return_some value
  in
  let on_open_error = function
    | Unix.Unix_error (e, _, _) -> Lwt.return_none
    | exn -> Lwt.fail exn
  in
  Lwt.try_bind do_open process_tty on_open_error

let tty_printl handle string =
  write_fd handle string

let tty_read_password fd prompt =
  write_fd fd prompt >>= fun () ->
  Lwt_unix.tcgetattr fd >>= fun tio ->

  (*
   * getpass in glibc disables both ECHO and ISIG.
   * ssh.terminal.ReadPassword in Go disables ECHO and
   * sets ICANON, ISIG, ICRNL. That seems more resonable,
   * so follow it, but also reset VEOL.
   *)
  let no_echo = Lwt_unix.({ tio with
                            c_echo = false;
                            c_icanon = true;
                            c_isig = true;
                            c_icrnl = true;
                            c_veol = '\x00';
                }) in
  Lwt_unix.tcsetattr fd Lwt_unix.TCSAFLUSH no_echo >>= fun () ->
  let reset_tty () = Lwt_unix.tcsetattr fd Lwt_unix.TCSAFLUSH tio in
  with_cleanup reset_tty @@ fun () ->
  read_tty_line fd >>= fun (full_line, str) ->

  (*
   * The tail \n if any in str was not echoed. Do it now that we have
   * restored the echo.
   *)
  if full_line then
    write_fd fd "\n" >>= fun() -> return str
  else
    return str

module Infix =
struct
  let (>>=) = Lwt.bind
end
include Infix
