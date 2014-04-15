open Async.Std

let init addrs =
  let aque = AQueue.create () in
  let rec add aque lst = match lst with
  	| [] -> ()
  	| h::t -> (AQueue.push aque h); (add aque t) in
  add aque addrs

module Make (Job : MapReduce.Job) = struct
  let map_reduce inputs =
    failwith "Nowhere special."

end

