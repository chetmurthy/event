
open Event

let buffer = Bytes.create 1024 ;;

let read_char loop fd kont =
  loop#in_handler fd
    (fun fd ->
       loop#in_cancel fd ;
       let nread = Unix.read fd buffer 0 1 in
       if nread = 0 then kont (-1)
       else kont (Char.code (Bytes.get buffer 0)))

let rec readn loop ib toread kont =
  if toread = 0 then kont toread
  else
    IBuffer.read_char loop ib
      (fun c ->
         if -1 = c then kont toread
         else readn loop ib (toread-1) kont)

let main() =
  let fname = Sys.argv.(1) in
  let bufsiz = int_of_string Sys.argv.(2) in
  let toread = int_of_string Sys.argv.(3) in
  let loop = new event_loop_t in
  let fd = Unix.openfile fname [Unix.O_RDONLY] 0o755 in
  let ibuf = IBuffer.create ~bufsiz fd in
  Unix.set_nonblock fd ;
  let stime = Unix.gettimeofday() in
  readn loop ibuf toread (fun unread ->
      let etime = Unix.gettimeofday() in
      Printf.printf "%d read in %f secs\n%!" (toread-unread) (etime -. stime) ;
      loop#exit 0) ;
  loop#loop

let _ = 
if not !Sys.interactive then
  main()
else ()
