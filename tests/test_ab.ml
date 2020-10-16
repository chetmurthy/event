
open Event

let buffer = Bytes.create 1024 ;;

let upd a b = a lor b
(*
let upd a b = 0
*)
module KontMode = struct

module Kont = struct
  type 'a comp = ('a -> unit) -> unit
  let return v k = k v
  let bind (_N : 'a comp) (_M : 'a -> 'b comp) = fun kont ->
    _N (fun v -> _M v kont)
end

let read1 n : int Kont.comp =
  Kont.return (Char.code (Bytes.get buffer (n mod 1024)))

let rec readn acc n : int Kont.comp =
  if n = 0 then Kont.return n
  else
    Kont.bind (read1 n)
      (fun c kont -> readn (upd c acc) (n-1) kont)

end

module DirectMode = struct

let rec readn acc n kont =
  if n = 0 then kont n
  else
    let c = Char.code (Bytes.get buffer (n mod 1024)) in
    readn (upd c acc) (n-1) kont

end

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n%!" bi_byte_size persec nread dur)

let main() =
  let mode = Sys.argv.(1) in
  let toread = int_of_string Sys.argv.(2) in
  let stime = Unix.gettimeofday() in
  if mode = "direct" then
  DirectMode.readn 0 toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime))
  else
  KontMode.readn 0 toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime))

let _ = 
if not !Sys.interactive then
  main()
else ()
