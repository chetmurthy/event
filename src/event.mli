val spawn : string -> string array -> int * Unix.file_descr * Unix.file_descr

type handler_t = Unix.file_descr -> unit
type handler_list_t

module Loop :
  sig
    type t = {
      in_handlers : handler_list_t;
      out_handlers : handler_list_t;
      exn_handlers : handler_list_t;
      mutable pid_handlers :
        (int * (int -> Unix.process_status -> unit)) list;
      mutable alarms : (int * (float * (int -> unit))) list;
      mutable exit_code : int;
    }
    val create : unit -> t
    val exit : t -> int -> unit
    val in_handler :
      t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit
    val out_handler :
      t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit
    val exn_handler :
      t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit
    val in_cancel : t -> Unix.file_descr -> unit
    val out_cancel : t -> Unix.file_descr -> unit
    val exn_cancel : t -> Unix.file_descr -> unit
    val fd_cancel : t -> Unix.file_descr -> unit
    val fd_handlers :
      t ->
      Unix.file_descr ->
      (Unix.file_descr -> unit) option * (Unix.file_descr -> unit) option *
      (Unix.file_descr -> unit) option
    val pid_handler :
      t -> int -> (int -> Unix.process_status -> unit) -> unit
    val pid_cancel : t -> int -> unit
    val pid_cancel_all : t -> unit
    val alarm : t -> float -> (int -> unit) -> int
    val handle_pid : t -> int * Unix.process_status -> unit
    val handle_deaths : t -> unit
    val loop : t -> unit
  end


class event_loop_t :
  object
    method loop : unit

    method exit : int -> unit
    method alarm : float -> (int -> unit) -> int

    method fd_cancel : Unix.file_descr -> unit
    method fd_handlers : Unix.file_descr -> (handler_t option * handler_t option * handler_t option)

    method exn_cancel : Unix.file_descr -> unit
    method exn_handler :
      Unix.file_descr -> (Unix.file_descr -> unit) -> unit

    method in_cancel : Unix.file_descr -> unit
    method in_handler :
      Unix.file_descr -> (Unix.file_descr -> unit) -> unit

    method out_cancel : Unix.file_descr -> unit
    method out_handler :
      Unix.file_descr -> (Unix.file_descr -> unit) -> unit

    method pid_cancel_all : unit
    method pid_cancel : int -> unit
    method pid_handler : int -> (int -> Unix.process_status -> unit) -> unit

    method private handle_pid : int * Unix.process_status -> unit
    val mutable alarms : (int * (float * (int -> unit))) list
    val exn_handlers : handler_list_t
    val in_handlers : handler_list_t
    val out_handlers : handler_list_t
    val mutable pid_handlers :
      (int * (int -> Unix.process_status -> unit)) list
  end
