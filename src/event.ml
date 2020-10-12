
open Arg

let spawn prog args =
  Printf.eprintf "<<%s>> <<%s>>\n" prog (String.concat ">> <<" (Array.to_list args)); flush stderr;
  let (in_read, in_write) = Unix.pipe () in
  let (out_read, out_write) = Unix.pipe () in
  let cpid = Unix.create_process prog args in_read out_write out_write in
    Unix.close in_read;
    Unix.close out_write;
    (cpid, in_write, out_read)

let next_token = ref 0
let next_token () =
  incr next_token;
  !next_token

type handler_t = Unix.file_descr -> unit
type handler_list_t =
    { mutable fds : Unix.file_descr list;
      mutable l :  (Unix.file_descr * (Unix.file_descr -> unit)) list }

let mk_handler_list () = {fds=[];l=[]}
let get_fds hl = hl.fds
let add_fd hl fd f =
  hl.l <- (fd,f)::hl.l;
  hl.fds <- List.map fst hl.l

let lookup_fd hl fd = List.assoc fd hl.l

let del_fd hl fd =
  hl.l <- List.remove_assoc fd hl.l;
    hl.fds <- List.map fst hl.l

class event_loop_t =

object(self)
  val in_handlers = mk_handler_list()
  val out_handlers = mk_handler_list()
  val exn_handlers = mk_handler_list()

  val mutable pid_handlers = []
  val mutable alarms = []
  val mutable exit_code = -1

  method exit code = exit_code <- code ; ()
  method in_handler fd f = add_fd in_handlers fd f
  method out_handler fd f = add_fd out_handlers fd f
  method exn_handler fd f = add_fd exn_handlers fd f

  method in_cancel fd = del_fd in_handlers fd
  method out_cancel fd = del_fd out_handlers fd
  method exn_cancel fd = del_fd exn_handlers fd

  method fd_cancel fd =
    self#in_cancel fd;
    self#out_cancel fd;
    self#exn_cancel fd

  method fd_handlers fd =
    ((try Some(lookup_fd in_handlers fd) with Not_found -> None),
     (try Some(lookup_fd out_handlers fd) with Not_found -> None),
     (try Some(lookup_fd exn_handlers fd) with Not_found -> None))

  method pid_handler pid f = pid_handlers <- (pid,f)::pid_handlers
  method pid_cancel pid = pid_handlers <- List.remove_assoc pid pid_handlers
  method pid_cancel_all = pid_handlers <- []

  method alarm t (f: int -> unit) =
  let n = next_token () in
  let rec addrec = function
      [] -> [(n,(t,f))]
    | ((n0,(t0,f0)) as h)::tl when t0 < t ->
	h::(addrec tl)
    | l -> (n,(t,f))::l
  in alarms <- addrec alarms;
    n

  method private handle_pid (pid,status) =
    if not(List.mem_assoc pid pid_handlers) then
      Fmt.(pf stderr "WARNING: [PID %d has no waiter]\n%!" pid)
    else
      let f = List.assoc pid pid_handlers in
	self#pid_cancel pid;
	f pid status

  method private handle_deaths =
    match Unix.waitpid [Unix.WNOHANG] (-1) with
	(0,_) -> ()
      | x -> self#handle_pid x; self#handle_deaths

  method loop =
    let _ : Sys.signal_behavior =
      Sys.signal Sys.sigchld
        (Sys.Signal_handle
           (fun _ ->
	      self#handle_deaths)) in
    let rec looprec () =
      if exit_code <> -1 then exit_code else
      let timeout =
	match alarms with
	    [] -> -1.0
	  | (_,(t,_))::_ -> t -. Unix.gettimeofday() in

      let ins = get_fds in_handlers in
      let outs = get_fds out_handlers in
      let exns = get_fds exn_handlers in
      match (try Some (Unix.select ins outs exns timeout)
	     with Unix.Unix_error((Unix.EAGAIN|Unix.EINTR), _, _) -> None) with
	  None -> looprec()
	| Some([],[],[]) ->
	    assert (timeout >= 0.0);
	    assert (alarms <> []); 
	    let (n,(_,f))::tl = alarms
	    in alarms <- tl;
	      f n;
	      looprec ()

	| Some(i_active, o_active, e_active) ->
	    let rec iter f = function
		[] -> ()
	      | a::l -> f a; iter f l
	    in
	      iter
		(fun fd ->
		   let f = lookup_fd in_handlers fd
		   in f fd)
		i_active;

	      List.iter
		(fun fd ->
		   let f = lookup_fd out_handlers fd
		   in f fd)
		o_active;

	      List.iter
		(fun fd ->
		   let f = lookup_fd exn_handlers fd
		   in f fd)
		e_active;

	      looprec ()
    in
      looprec ();
      ()
end
