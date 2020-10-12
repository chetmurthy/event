
let buffer = Bytes.create 1024 ;;

let rec readn n =
  if n = 0 then Lwt.return n
  else
    Lwt.bind (Lwt.return (Bytes.get buffer (n mod 1024)))
      (fun _ -> readn (n-1))

let bench f n =
  let stime = Unix.gettimeofday() in
  Lwt.bind (Lwt.return ())
    (fun _ ->
       Lwt.bind (readn n)
         (fun unread ->
           let etime = Unix.gettimeofday() in
           Stdlib.Printf.printf "%d read in %f secs\n%!" (n-unread) (etime -. stime) ;
           Lwt.return ()
         )
    )
;;

Lwt_main.run (bench Sys.argv.(1) (int_of_string Sys.argv.(2))) ;;
