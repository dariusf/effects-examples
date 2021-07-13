
(* continuing after handling a Foo returns a (unit -> 'a) *)
effect Foo : (unit -> 'a)

(* f on its own is not divergent. Also we want modular specs,
   so we want some kind of description not just of the effects of f,
   but of what they depend on/of their surrounding context in f *)

(* requires emp *)
(* ensures  Foo.(apply ()) *)
(* made-up syntax for "effects from applying () to a value of type unit -> 'a" *)
let f () =
  (* emp *)
  let g = perform Foo in
  (* Foo *)
  g ()
  (* Foo.(apply ()) *)

let res =
  match f () with
  | x -> x
  | effect Foo k ->
    print_endline "asd";
    (* spec of f: Foo.(apply ()) *)
    (* spec of g: Foo.(apply ()) *)
    let g = (fun () -> perform Foo ()) in
    (* policy: Foo -> (apply () to (unit -> Foo.(apply ()))) *)
    (* policy: Foo -> (Foo.(apply ())) *)
    (* there is a cycle because from a Foo, we know we'll get back to this handler unconditionally *)
    continue k g
