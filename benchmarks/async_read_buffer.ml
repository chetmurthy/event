
open Async ;;

let buffer = Bytes.create 1024 ;;

let rec readn n =
  if n = 0 then Deferred.return n
  else
    Deferred.bind (Deferred.return (Bytes.get buffer (n mod 1024)))
      (fun _ -> readn (n-1))

let bench f n =
  let stime = Unix.gettimeofday() in
  Deferred.bind (Deferred.return ())
    (fun () -> Deferred.bind (readn n)
        (fun unread ->
           let etime = Unix.gettimeofday() in
           Stdlib.Printf.printf "%d read in %f secs\n%!" (n-unread) (etime -. stime) ;
           Shutdown.exit 0
        ))
;;
bench (Sys.get_argv()).(1) (int_of_string (Sys.get_argv()).(2)) ;;
Scheduler.go() ;;
