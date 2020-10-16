
let buffer = Bytes.create 1024 ;;

let rec readn n =
  if n = 0 then n
  else
    let _ = Bytes.get buffer (n mod 1024) in
    readn (n-1)

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n\n%!" bi_byte_size persec nread dur)

let bench f n =
  let stime = Unix.gettimeofday() in
  let unread = readn n in
  let etime = Unix.gettimeofday() in
  report (n-unread) (etime -. stime) ;
  ()
;;

bench Sys.argv.(1) (int_of_string Sys.argv.(2)) ;;
