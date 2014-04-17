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
	let rec assignJobs js = function
      	  | (s,r,w)::t -> Writer.write_line w Job.name; assignJobs t
          | [] -> () in
  	let rec maphelper lst =
  	  (* do something *)
  	  (* repeat for reduce *)
  	  in
  	let collect keyil = 
	        let lcompare a b = 
	          if fst(a) < fst(b) then -1
	          else if fst(a) = fst(b) then 0
	          else 1
	        in
	        let rec listify kyil acc = 
	          match kyil with
	            | (k,i)::t -> listify t ((k,[i])::acc)
	            | [] -> acc
	        in
	        let rec combine lfied acc = 
	          match lfied with
	            | (k,il)::t -> (
	              if acc = [] then combine t [(k,il)]
	              else if k = fst(List.hd acc) then
	                combine t ((k,(il @ (snd(List.hd acc))))::(List.tl acc))
	              else combine t ((k,il)::acc)
	            )
	            | [] -> acc
	        in combine (listify (List.sort lcompare keyil) []) [] in
	let rec reducehelper lst = 
	  (*do something similar to map*)
	  in
	(*need to establish worker connections first, not sure how*)
	(maphelper inputs) >= (fun mres -> reducehelper (collect mres))
	(*not sure what the entire map_reduce should return*)
end
