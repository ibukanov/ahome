open Iokit
open Iokit.Infix

let password_bcrypt_cost = 11

let password_get_title optional_title path =
  match optional_title with
  | Some title -> title
  | None -> path

let password_ask (title : string option) (path : string) : unit Iokit.t =
  let get_hash =
    Iokit.read_file_fully path >>= fun hash_str ->
    return (Bcrypt.hash_of_string hash_str)
  in
  let check_password : string option Iokit.t =
    let ask_on_tty fd =
      let prompt =
        str "Enter %s password: " (password_get_title title path)
      in
      tty_read_password fd prompt >>= fun password ->
      if String.length password == 0 then user_cancel ()
      else
        get_hash >>= fun bcrypt_hash ->
        if Bcrypt.verify password bcrypt_hash then
           return_some password
        else
          tty_printl fd "Password does not match the hash." >>= fun () ->
          return_none
    in
    with_tty ask_on_tty >>= fun optional_tty_result ->
    match optional_tty_result with
    | Some tty_result -> return tty_result
    | None -> err "Asking for password without tty is not supported"
  in
  let write_result password = match password with
    (*
     * We do not need the hash result as it is already consumed in
     * check_password.
     *)
    | Some password -> write_stdout password
    | None -> user_cancel ()
  in
  let ignore_result promise = promise >>= fun _ -> return_unit in
  safe_join2 (ignore_result get_hash) (check_password >>= write_result)

let password_set_hash title path =
  let write_hash password =
    let bcrypt_hash =
      Bcrypt.hash ~count:password_bcrypt_cost password
    in
    write_file path bcrypt_hash
  in
  let ask_new_on_tty fd =
    let title = password_get_title title path in
    let prompt = str "Enter a new password for %s: " title in
    tty_read_password fd prompt >>= fun password ->
    if String.length password = 0 then
        tty_printl fd "Cancelled" >>= fun () -> return_false
    else
      let prompt =
        str "Repeat to confirm the new value for %s: " title
      in
      tty_read_password fd prompt >>= fun password2 ->
      if String.length password2 = 0 then
        tty_printl fd "Cancelled" >>= fun () -> return_false
      else if password2 <> password then
        tty_printl fd "Password mismatch" >>= fun () -> return_false
      else
        write_hash password >>= fun () -> return_true
  in
  with_tty ask_new_on_tty >>= function
  | Some true -> write_stdout "done\n"
  | Some false -> write_stdout "canceled\n"
  | None -> err "Setting password hash without tty is not implemented"


open Cmdliner

let exit_status_io_error = 1
let exit_status_user_cancel = 2

let exit_codes =
  let error_exit =
    let doc = "IO error" in
    Term.exit_info exit_status_io_error ~doc
  in
  let user_cancel_exit =
    let doc = "The action of utility was canceled by user." in
    Term.exit_info exit_status_user_cancel ~doc
  in
  error_exit :: user_cancel_exit :: Term.default_exits

let password_terms =
  let hash_path =
    let doc = "Path for the password hash file." in
    let inf = Arg.info [] ~docv:"HASH_PATH" ~doc in
    Arg.(required & pos 0 (some string) None & inf)
  in
  let title =
    let doc =
      "Title for the password. Defaults to the name of the password \
      hash file."
    in
    let inf = Arg.info ["t"; "title"] ~docv:"TITLE" ~doc in
    Arg.(value & opt (some string) None & inf)
  in
  let ask_password_cmd =
    let doc =
      "asks for password and print password<NL>PASSWORD-VALUE it if it \
      matches the hash. Prints canceled<NL> if the password mismatches \
      the hash or if the user canceled the password entering"
    in
    Term.(const password_ask $ title $ hash_path),
    Term.info "ask-password" ~doc ~exits:exit_codes
  in
  let set_password_hash_cmd =
    let doc =
      "asks for the new password and writes its hash. Prints done<NL> \
      on success, canceled<NL> if user cancels or mistyped the \
      password."
    in
    Term.(const password_set_hash $ title $ hash_path),
    Term.info "set-password-hash" ~doc ~exits:exit_codes
  in
  [ ask_password_cmd; set_password_hash_cmd ]


let default_cmd =
  let doc =
    "Collection of utilities to call from shell scripts"
  in
  Term.(ret (const (fun () -> `Help (`Auto, None)) $ const ())),
  Term.info "u-kit" ~doc ~exits:Term.default_exits


let () =
  let report_exception exn = prerr_endline (Printexc.to_string exn) in
  let evaluate_promise promise =
    try
      let () = run_io promise in 0
    with
    | UserCancelException -> exit_status_user_cancel
    | exn ->
      report_exception exn;
      match exn with
      | IOErrorException _ -> exit_status_io_error
      | Unix.Unix_error _ -> exit_status_io_error
      | exn -> Term.exit_status_internal_error
  in
  match Term.eval_choice default_cmd password_terms with
  | `Ok promise -> exit (evaluate_promise promise)
  | other_eval_result -> Term.exit other_eval_result
