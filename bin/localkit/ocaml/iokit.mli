
type error_kind =
  | Too_Big_Read [@@deriving sexp]

exception IOErrorException of error_kind

exception UserCancelException

exception Exception_List of exn list

type 'a t

val run_io: 'a t -> 'a

val return: 'a -> 'a t
val return_unit: unit t
val return_false: bool t
val return_true: bool t
val return_none: 'a option t
val return_some: 'a -> 'a option t

val err: string -> 'a t

val user_cancel: unit -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val str: ('a, unit, string) format -> 'a

val safe_join2: unit t -> unit t -> unit t

val read_file_fully: string -> string t

val write_stdout: string -> unit t

val write_file: string -> string -> unit t

type tty_handle

val with_tty: (tty_handle -> 'a t) -> 'a option t

val tty_read_password: tty_handle -> string -> string t

val tty_printl: tty_handle -> string -> unit t

module Infix : sig

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [t >>= f] is an alternative notation for [bind t f]. *)

end
