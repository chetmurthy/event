open Stdlib ;;

let rec readn ic n =
  if n = 0 then n
  else
    let _ = input_char ic in
    readn ic (n-1)

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n%!" bi_byte_size persec nread dur)

let bench f n =
  let stime = Unix.gettimeofday() in
  let ic = open_in f in
  let unread = readn ic n in
  let etime = Unix.gettimeofday() in
  report (n-unread) (etime -. stime) ;
  ()
;;

bench Sys.argv.(1) (int_of_string Sys.argv.(2)) ;;
