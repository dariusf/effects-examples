
(* Simple promises (no awaiters) *)

type 'a state =
  | Done of 'a
  | Err of exn
  | Empty

type 'a promise =
  { mutable result: 'a state }

(* Scheduler queue, which contains (unit -> unit) functions *)

let run_q = Queue.create ()
let enqueue f = Queue.push f run_q
let dequeue () =
  if Queue.is_empty run_q then None
  else Some (Queue.pop run_q)

(* Async/await *)

(** async creates a promise and enqueues an action that completes it *)
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

(* Await is the only effect *)

effect Await : unit

(** Given a promise, await returns/raises if it's done, or
    repeatedly yields control until it's done. *)
let rec await (p : 'a promise) : 'a =
  match p.result with
  | Done v -> v
  | Err e -> raise e
  | Empty ->
    perform Await;
    await p

(** The scheduler's event loop (spawn) goes through the queue,
    yielding when it encounters an Await *)
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
        enqueue (continue k);
        spawn d
  in
  spawn main

let () =
  let main () =
    (* p, q, and r are all promises. q fails and is awaited by p, so p fails as well *)
    let q = async (fun () -> failwith "dead") in
    let p = async (fun () ->
      let v = await q in
      let r = async (fun () -> 4) in
      let v1 = await r in
      v1 + v + 3) in

    (* Exceptions should surface only when the top-level promise is awaited *)
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
