open Lwt






(*

let test_file_read path block_size = Lwt.(
    let read_fd fd =
      let buffer_size = block_size in
      let buffer = Bytes.create buffer_size in
      let start_time = Unix.gettimeofday() in
      let rec read_chunks sum =
        Lwt_unix.read fd buffer 0 buffer_size >>= fun nread ->
        if nread = 0 then return sum
        else
          Lwt_unix.yield () >>= fun () ->
          read_chunks (sum +. (float_of_int nread))
      in
      read_chunks 0.0 >>= fun total ->
      let duration = Unix.gettimeofday() -. start_time in
      let totalMB = total /. (1024.0 *. 1024.0) in
      Printf.sprintf "Total read: %.3f MB, speed: %.3f MB/s" totalMB (totalMB /. duration)
      |> Lwt_io.printl
    in
    Lwt_unix.(openfile path [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0) >>= fun fd ->
    finalize (fun () -> read_fd fd) (fun () -> Lwt_unix.close fd)
  )

let timer n = Lwt.(
    let rec tick n () =
      if n <= 0 then return_unit
      else
        Lwt_io.printl (string_of_int n) >>= fun() ->
        let step = min 5 n in
        Lwt_unix.sleep (float step) >>=
        (tick (n - step))
    in
    tick n ()
  )

*)

(*
Lwt.join [test_file_read "/var/tmp/zeros"; timer 5]
*)
