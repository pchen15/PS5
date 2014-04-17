open Async.Std

module Make (Job : MapReduce.Job) = struct

  (* see .mli *)
  let run r w =
    let job = Protocol.receive r in
    job >>| function
      | `Ok someJob -> (match someJob with 
        | MapRequest mapJob ->
            let mapRes = WorkerResponse.MapResult (MapReduce.Job.map mapJob) in
            mapRes >>| (fun res -> Protocol.send w res)
        | ReduceRequest reduceJob ->
          let redRes = WorkerResponse.ReduceResult (MapReduce.Job.reduce reduceJob) in
          redRes >>| (fun res -> Protocol.send w res)
        | _ -> failwith "stuff")
      | _ -> failwith "no job"
      (* replace all failwiths with error messages that the controller will handle *)
      (* do something about the job name *)

end

(* see .mli *)
let init port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.on_port port)
    (fun _ r w ->
      Reader.read_line r >>= function
        | `Eof    -> return ()
        | `Ok job -> match MapReduce.get_job job with
          | None -> return ()
          | Some j ->
            let module Job = (val j) in
            let module Mapper = Make(Job) in
            Mapper.run r w
    )
    >>= fun _ ->
  print_endline "server started";
  print_endline "worker started.";
  print_endline "registered jobs:";
  List.iter print_endline (MapReduce.list_jobs ());
  never ()


