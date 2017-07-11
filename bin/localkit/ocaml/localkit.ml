open Lwt
open Iokit

let str = Printf.sprintf

let password_read_stdin () =
  Iokit.read_descriptor_fully Lwt_unix.stdin

let password_bcrypt_prefix = "bcrypt:"
let password_bcrypt_cost = 11

let password_get_title optional_title path =
  match optional_title with
  | Some title -> title
  | None -> path

let password_ask (title : string option) (path : string) : unit Lwt.t =
  (* Start password has reading in parallel with asking the password *)
  let password_hash_promise =
    Iokit.read_file_fully path >>= fun hash_str ->
    let nprefix = String.length password_bcrypt_prefix in
    let nhash = String.length hash_str in
    let prefix_match =
      nhash >= nprefix
      && password_bcrypt_prefix = String.sub hash_str 0 nprefix
    in
    if not prefix_match then
      err (str "Invalid password hash format - it must start with %s"
           password_bcrypt_prefix)
    else
      let bcrypt_str = String.sub hash_str nprefix (nhash - nprefix) in
      return (Bcrypt.hash_of_string bcrypt_str)
  in
  let write_result = function
    | Some password -> write_stdout (str "password\n%s" password)
    | None -> write_stdout "canceled\n"
  in
  let ask_on_tty fd =
    let prompt =
      str "Enter %s password: " (password_get_title title path)
    in
    read_tty_password fd prompt >>= fun password ->
    password_hash_promise >>= fun bcrypt_hash ->
    if Bcrypt.verify password bcrypt_hash then
       return (Some password)
    else
      write_fd fd "Password does not match the hash.\n" >>= fun () ->
      return_none
  in
  let ask () =
    with_tty ask_on_tty >>= function
    | Some result -> write_result result
    | None -> err "Asking for password without tty is not implemented"
  in
  use_or_cancel password_hash_promise ask

let password_set_hash title path =
  let write_hash password =
    let bcrypt_hash =
      Bcrypt.hash ~count:password_bcrypt_cost password
    in
    let hash_str =
      password_bcrypt_prefix ^ Bcrypt.string_of_hash bcrypt_hash
    in
    write_file path hash_str
  in
  let ask_new_on_tty fd =
    let title = password_get_title title path in
    let prompt = str "Enter a new password for %s: " title in
    read_tty_password fd prompt >>= fun password ->
    if String.length password = 0 then
        write_fd fd "Cancelled\n" >>= fun () -> return_false
    else
      let prompt =
        str "Repeat to confirm the new value for %s: " title
      in
      read_tty_password fd prompt >>= fun password2 ->
      if String.length password2 = 0 then
        write_fd fd "Cancelled\n" >>= fun () -> return_false
      else if password2 <> password then
        write_fd fd "Password mismatch\n" >>= fun () -> return_false
      else
        write_hash password >>= fun () -> return_true
  in
  with_tty ask_new_on_tty >>= function
  | Some true -> write_stdout "done\n"
  | Some false -> write_stdout "canceled\n"
  | None -> err "Setting password hash without tty is not implemented"


open Cmdliner

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
    Term.info "ask-password" ~doc
  in
  let set_password_hash_cmd =
    let doc =
      "asks for the new password and writes its hash. Prints done<NL> \
      on success, canceled<NL> if user cancels or mistyped the \
      password."
    in
    Term.(const password_set_hash $ title $ hash_path),
    Term.info "set-password-hash" ~doc
  in
  [ ask_password_cmd; set_password_hash_cmd ]


let default_cmd =
  let doc =
    "Collection of utilities to call from shell scripts"
  in
  Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ())),
  Term.info "u-kit" ~doc

(*
 * TODO switch
 *)
let () =
  match Term.eval_choice default_cmd password_terms with
  | `Ok v -> Lwt_main.run v
  | _ -> raise (Failure "NOT IMPLEMENTED error handling")
