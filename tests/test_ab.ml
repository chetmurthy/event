
open Event

let buffer = Bytes.create 1024 ;;


module KontMode = struct

module Kont = struct
  let return v k = k v
  let bind _N _M = fun kont ->
    _N (fun v -> _M v kont)
end

let read1 n =
  Kont.return (Bytes.get buffer (n mod 1024))

let rec readn n =
  if n = 0 then Kont.return n
  else
    Kont.bind (Kont.return (Bytes.get buffer (n mod 1024)))
      (fun _ -> readn (n-1))

end

module DirectMode = struct

let rec readn n kont =
  if n = 0 then kont n
  else
    let _ = Bytes.get buffer (n mod 1024) in
    readn (n-1) kont

end

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n%!" bi_byte_size persec nread dur)

let main() =
  let mode = Sys.argv.(1) in
  let toread = int_of_string Sys.argv.(2) in
  let stime = Unix.gettimeofday() in
  if mode = "direct" then
  DirectMode.readn toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime))
  else
  KontMode.readn toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime))

let _ = 
if not !Sys.interactive then
  main()
else ()
