
open Async ;;

let rec readn ic n =
  if n = 0 then Deferred.return n
  else
    Deferred.bind (Reader.read_char ic)
      (function `Ok _ -> readn ic (n-1)
              | `Eof -> Deferred.return n)

let bench f ~buf_len n =
  let stime = Unix.gettimeofday() in
  Deferred.bind (Reader.open_file ~buf_len:buf_len f)
    (fun ic -> Deferred.bind (readn ic n)
        (fun unread ->
           let etime = Unix.gettimeofday() in
           Stdlib.Printf.printf "%d read in %f secs\n%!" (n-unread) (etime -. stime) ;
           Shutdown.exit 0
        ))
;;
bench (Sys.get_argv()).(1) ~buf_len:(int_of_string (Sys.get_argv()).(2)) (int_of_string (Sys.get_argv()).(3)) ;;
Scheduler.go() ;;
