
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
  fun kont ->
  Kont.return (Char.code (Bytes.get buffer (n mod 1024))) kont

let rec readn acc n : int Kont.comp =
  fun kont -> 
  if n = 0 then Kont.return n kont
  else
    Kont.bind (read1 n)
      (fun c kont -> readn (upd c acc) (n-1) kont) kont


let doit toread kont =
  readn 0 toread kont

end

module KontMode2 = struct

module Kont = struct
  type 'a comp = ('a -> unit) -> unit
  let return v k = k v
  let bind (_N : 'a comp) (_M : 'a -> 'b comp) = fun kont ->
    _N (fun v -> _M v kont)
end

let read1 n : int Kont.comp =
  fun kont -> 
  Kont.return (Char.code (Bytes.get buffer (n mod 1024))) kont

let rec readn acc n : int Kont.comp =
  fun kont -> 
  if n = 0 then Kont.return n kont
  else
    Kont.bind (read1 n)
      (fun c kont -> readn (upd c acc) (n-1) kont) kont


let doit toread kont =
  readn 0 toread kont

end

module DirectMode = struct

let rec readn acc n kont =
  if n = 0 then kont n
  else
    let c = Char.code (Bytes.get buffer (n mod 1024)) in
    readn (upd c acc) (n-1) kont

let doit toread kont =
  readn 0 toread kont

end

module DirectMode2 = struct

let rec readn loop ibuf acc n kont =
  if n = 0 then kont n
  else
    let c = DirectJustBuffer.read_char loop ibuf in
    readn loop ibuf (upd c acc) (n-1) kont

let doit loop ibuf toread kont =
  readn loop ibuf 0 toread kont

end

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n\n%!" bi_byte_size persec nread dur)

let main() =
  let mode = Sys.argv.(1) in
  let bufsiz = int_of_string Sys.argv.(2) in
  let toread = int_of_string Sys.argv.(3) in
  let stime = Unix.gettimeofday() in
  let loop = Loop.create () in
  if mode = "direct" then
    DirectMode.doit toread (fun unread ->
        let etime = Unix.gettimeofday() in
        report  (toread-unread) (etime -. stime))
  else if mode = "direct2" then
    let ibuf = DirectJustBuffer.create ~bufsiz in
    DirectMode2.doit loop ibuf toread (fun unread ->
        let etime = Unix.gettimeofday() in
        report  (toread-unread) (etime -. stime))
  else if mode = "kont" then
    KontMode.doit toread (fun unread ->
        let etime = Unix.gettimeofday() in
        report  (toread-unread) (etime -. stime))
  else if mode = "kont2" then
    KontMode2.doit toread (fun unread ->
        let etime = Unix.gettimeofday() in
        report  (toread-unread) (etime -. stime))
  else ()
let _ = 
if not !Sys.interactive then
  main()
else ()
