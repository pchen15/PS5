open Async.Std

let address = ref []

let init addrs =
  address := addrs

module Make (Job : MapReduce.Job) = struct
  let map_reduce inputs =
  	let workers = AQueue.create () in
  	let worker_list = Warmup.deferred_map address (fun host, port ->
	  Tcp.connect (to_host_and_port host port)) in
  	let rec add_to_queue lst = match lst with
	  | [] -> ()
	  | h::t -> AQueue.push workers h; add_to_queue t in
	add_to_queue worker_list;
  	let rec maphelper lst =
  	  (* do something *)
  	  (* repeat for reduce *)

end