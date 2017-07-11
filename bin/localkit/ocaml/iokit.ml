open Lwt

let max_memory_read_file_size = 1 lsl 30
let max_io_chunk = 16 * 1024

type error_kind = | Too_Big_Read [@@deriving sexp]

exception Exception of error_kind [@@deriving sexp]

exception Exception_List of exn list [@@deriving sexp]

let combine_exceptions a b =
  if a = b then a
  else
    let list = match (a, b) with
      | (Exception_List list, Exception_List list2)
        -> List.append list list2
      | (Exception_List list, e) -> List.append list [e]
      | (e, Exception_List list) -> e :: list
      | (e1, e2) -> [e1; e2]
    in
    Exception_List list

let ok_or_error (action: unit -> 'a Lwt.t) : ('a, exn) Result.result Lwt.t =
  catch (fun () -> action () >>= fun x -> return (Result.Ok x))
    (fun e -> return (Result.Error e))

let safe_finalize body finalizer =
  ok_or_error body >>= function
  | Result.Ok x -> finalizer () >>= fun () -> return x
  | Result.Error e ->
    let handle_failed_finalizer e2 = fail (combine_exceptions e e2) in
    catch finalizer handle_failed_finalizer >>= fun _ -> fail e

let ok_or_unix_error action =
  let action_wraper () = action () >>= fun x -> return (Result.Ok x) in
  let error_handler = function
    | Unix.Unix_error (e, _, _) -> return (Result.Error e)
    | exn -> fail exn
  in
  catch action_wraper error_handler

let use_or_cancel promise action =
  let close_promise () =
    (* Try to cancel promise and then wait for its result if any. *)
    let () = cancel promise in
    try_bind
      (fun () -> promise)
      (fun _ -> return_unit)
      (function | Canceled -> return_unit | e -> fail e)
  in
  safe_finalize action close_promise


let err text = fail (Failure text)

(*
 * Convenient function to close the file descriptor after some action.
 *)
let with_fd_close fd action =
  safe_finalize (fun () -> action fd) (fun () -> Lwt_unix.close fd)


let with_opened_file path flags permissions action =
  Lwt_unix.openfile path flags permissions >>= fun fd ->
  with_fd_close fd action

let read_file_fully path =
    with_opened_file path Lwt_unix.([ O_RDONLY; O_NONBLOCK; O_CLOEXEC ]) 0 (fun fd ->
      Lwt_unix.LargeFile.fstat fd >>= fun stats ->
      let file_size = Lwt_unix.LargeFile.(stats.st_size) in
      if Int64.compare file_size (Int64.of_int max_memory_read_file_size) > 0
      then fail (Exception Too_Big_Read)
      else
        let n = Int64.to_int file_size in
        if n = 0 then return ""
        else
          let buffer = Bytes.create n in
          let offset = ref 0 in
          let rec read_chunk () =
              let chunk_size = min max_io_chunk (n - !offset) in
              Lwt_unix.read fd (Bytes.unsafe_to_string buffer) !offset chunk_size >>= process_read_result
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
    )

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
        Lwt_unix.read fd (Bytes.unsafe_to_string data.chunk) data.size remaining >>= process_read_result
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
          fail (Exception Too_Big_Read)
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
          (*
           * Lwt_unix.write_string is not available in older Lwt, so
           * use Lwt_unix.write and assume bytes = string.
           *)
          Lwt_unix.write fd text !offset remaining >>= process_write_result
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
        with_fd_close tmp_fd (fun fd -> write_fd fd text) >>= fun () ->
        Lwt_unix.rename tmp_path path
      in
    catch write_rename (fun e -> Lwt_unix.unlink tmp_path >>= fun () -> fail e)

let lwt_bytes_to_string buffer offset length =
    let tmp = Bytes.create length in

    (* Compatibility with older LWT that assume string is bytes *)
    let () = Lwt_bytes.blit_bytes_string buffer offset (Bytes.unsafe_to_string tmp) 0 length in
    Bytes.unsafe_to_string tmp

(*
 * utility to read a line from a tty terminal when the read does not
 * return anything extra after the new line. Return (full_line, str)
 * where full_line is true if the full line was read. false indicates
 * eol was hit before the end-of-line. str never includes the line
 * terminator.
 *)
let read_tty_line fd : (bool * string) Lwt.t =
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
  let open_tty () =
    Lwt_unix.(openfile "/dev/tty" [O_NONBLOCK; O_CLOEXEC; O_RDWR] 0)
  in
  ok_or_unix_error open_tty >>= function
  | Result.Ok tty_fd -> with_fd_close tty_fd action >>= fun result ->
    return (Some result)
  | Result.Error _ -> return_none

let read_tty_password fd prompt =
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
  safe_finalize (fun () -> read_tty_line fd) reset_tty
  >>= fun (full_line, str) ->

  (*
   * The tail \n if any in str was not echoed. Do it now that we have
   * restored the echo.
   *)
  if full_line then
    write_fd fd "\n" >>= fun() -> return str
  else
    return str
