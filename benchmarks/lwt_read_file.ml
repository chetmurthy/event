
let rec readn ic n =
  if n = 0 then Lwt.return n
  else
    Lwt.bind (Lwt_io.read_char ic)
      (fun _ -> readn ic (n-1))

let bench  f ~buf_len n =
  let stime = Unix.gettimeofday() in
  let buffer = Lwt_bytes.create buf_len in
  Lwt.bind (Lwt_io.(open_file ~mode:input ~buffer f))
    (fun ic ->
       Lwt.bind (readn ic n)
         (fun unread ->
           let etime = Unix.gettimeofday() in
           Stdlib.Printf.printf "%d read in %f secs\n%!" (n-unread) (etime -. stime) ;
           Lwt.return ()
         )
    )
;;

Lwt_main.run (bench Sys.argv.(1) ~buf_len:(int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3))) ;;
