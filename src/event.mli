val spawn : string -> string array -> int * Unix.file_descr * Unix.file_descr

type handler_t = Unix.file_descr -> unit
type handler_list_t

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
