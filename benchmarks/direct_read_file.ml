open Stdlib ;;

let rec readn ic n =
  if n = 0 then n
  else
    let _ = input_char ic in
    readn ic (n-1)

let bench f n =
  let stime = Unix.gettimeofday() in
  let ic = open_in f in
  let unread = readn ic n in
  let etime = Unix.gettimeofday() in
  Stdlib.Printf.printf "%d read in %f secs\n%!" (n-unread) (etime -. stime)
;;

bench Sys.argv.(1) (int_of_string Sys.argv.(2)) ;;
