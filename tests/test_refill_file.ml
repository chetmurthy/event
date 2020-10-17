
open Event

let buffer = Bytes.create 1024 ;;

let rec readn loop ib toread kont =
  if toread <= 0 then kont toread
  else
    IBuffer.read_char loop ib
      (fun c ->
         if -1 = c then kont toread
         else
           let cap = IBuffer.drain ib in
           readn loop ib (toread-1-cap) kont)

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n\n%!" bi_byte_size persec nread dur)

let main() =
  let fname = Sys.argv.(1) in
  let bufsiz = int_of_string Sys.argv.(2) in
  let toread = int_of_string Sys.argv.(3) in
  let loop = Loop.create () in
  let fd = Unix.openfile fname [Unix.O_RDONLY] 0o755 in
  let ibuf = IBuffer.create ~bufsiz fd in
  Unix.set_nonblock fd ;
  let stime = Unix.gettimeofday() in
  readn loop ibuf toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime) ;
      Loop.exit loop 0) ;
  Loop.loop loop

let _ = 
if not !Sys.interactive then
  main()
else ()
