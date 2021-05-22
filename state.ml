open Printf

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t * 'a
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  effect Put : t -> unit
  let put v = perform (Put v)

  effect Get : t
  let get () = perform Get

  let run f ~init =
    let comp =
      match f () with
      | x -> (fun s -> (s, x))
      | effect (Put s') k ->
        (* ignore the current state, continue with unit, and pass s'
           as the value of the current state to the next effect.

           this takes advantage of the control flow of the handler
           returning; the continuation is never invoked so the function
           here is returned from the _previous_ continue that made f
           resume and raise the _current_ effect. *)
        (fun _s -> (continue k ()) s')
      | effect Get k ->
        (* the parameter is the previous value of the state,
           initially the initial value, and after that given by
           the _previous_ effect.

           parenthesizing the continue call makes this clearer. *)
        (fun s -> (continue k s) s)
    in
    comp init
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

(* (IS.get . IS.put)^2 . IS.get . (SS.put . SS.get)^2 *)
let foo () : unit =
  printf "%d\n" (IS.get ());
  IS.put 42;
  printf "%d\n" (IS.get ());
  IS.put 21;
  printf "%d\n" (IS.get ());
  SS.put "hello";
  printf "%s\n" (SS.get ());
  SS.put "world";
  printf "%s\n" (SS.get ())

(* ('s, 'a) *)
(* ('s, ('s1, 'a1)) *)
let _ : int * (string * unit) = IS.run (fun () -> SS.run foo ~init:"") ~init:0
