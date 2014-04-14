open Async.Std

let fork d f1 f2 =
  ignore (d >>= f1);
  ignore (d >>= f2);
  ()

let deferred_map l f =
	let map_of_def = List.map f l in
	let rec helper l =
		match l with
			| [] -> return []
			| h::t -> helper t >>= (fun l -> h >>= (fun h -> return ([h]@l))) in
	helper map_of_def
