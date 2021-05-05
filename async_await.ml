
effect Await : unit

type 'a state =
  | Done of 'a
  | Err of exn
  | Empty

type 'a promise =
  { mutable result: 'a state }

let run_q = Queue.create ()
let enqueue k = Queue.push k run_q
let dequeue () =
  if Queue.is_empty run_q then None
  else Some (Queue.pop run_q)

let async (f : unit -> 'a) : 'a promise =
  let p : 'a promise = { result = Empty } in
  let complete () : unit =
    p.result <-
      try
        Done (f ())
      with e ->
        Err e
  in
  enqueue complete;
  p

let rec await (p : 'a promise) : 'a =
  match p.result with
  | Done v -> v
  | Err e -> raise e
  | Empty ->
    perform Await;
    await p

let run (main : unit -> unit) : unit =
  let rec spawn f =
    match f () with
    | () ->
      begin
      match dequeue () with
      | None -> ()
      | Some d -> spawn d
      end
    | effect Await k ->
      match dequeue () with
      | None -> continue k ()
      | Some d ->
        enqueue (fun () -> continue k ());
        spawn d
  in
  spawn main

let () =
  let main () =
    let q = async (fun () -> 1) in
    let p = async (fun () ->
      let v = await q in
      let p1 = async (fun () -> 3) in
      let v1 = await p1 in
      v1 + v + 3) in
    (* let p : int promise = async (fun () -> 1) in *)
    (* let p : int promise = async (fun () -> failwith "test") in *)
    let res =
      try
        await p
      with _ ->
        10
    in
    print_endline (string_of_int res)
  in
  run main;
  print_endline "done";
