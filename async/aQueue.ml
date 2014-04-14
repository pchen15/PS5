open Async.Std

type 'a t

let create () =
  Pipe.create ()

let push q x =
  don't_wait_for (Pipe.write (snd q) x)

let pop  q =
  let r = Pipe.read (fst q) in
  r >>| function
  	| `Ok n -> n
  	| _ -> failwith "empty queue"

