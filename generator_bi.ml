
(* Mutable-nullable-product-list + utility functions *)

type 'a lst = {
  mutable head : 'a;
  mutable tail : 'a lst option;
}

let rec iter1 f {head; tail} =
  f head;
  match tail with
  | None -> ()
  | Some t -> iter1 f t

let rec to_lst xs =
  match xs with
  | [] -> None
  | x :: xs1 -> Some { head = x; tail = to_lst xs1 }

let get = Option.get

(* Iterator library *)

(* Ways in which client code can act on yielded elements *)

type 'a action
  = ToContinue
  | ToReplace of 'a
  | ToBehead

(* Replace is no longer expressed using an effect *)

effect Yield : int -> int action

effect Behead : unit

(* ensures Yield *)
let yield x = perform (Yield x)

(* ensures Behead *)
let behead () = perform (Behead)

(* ensures \x -> Yield.Behead?.x *)
(* ensures (Yield.Behead?)^* *)
let rec iter xs =
  begin
    match yield xs.head with
    | ToContinue -> ()
    | ToReplace v -> xs.head <- v
    | ToBehead -> behead ()
  end;
  match xs.tail with
  | None -> ()
  | Some tl ->
    try
      iter tl
    with effect Behead k ->
      (* policy: Behead -> (Yield.Behead?)^* *)
      xs.tail <- tl.tail;
      continue k ()

(* Client code *)

let () =
  let xs = ref (to_lst [-1; 2; -3; 4; 5]) in
  !xs |> get |> iter1 (fun x ->
    print_endline (string_of_int x)
  );
  begin
    try
      iter (!xs |> get)
    with
      | effect (Yield v) k ->
        (* Yield -> Behead?(Yield.Behead?)^* *)
        if v < 0 then (
          Format.printf "Beheading %d@." v;
          continue k ToBehead
        ) else (
          Format.printf "Replacing %d with %d@." v (v * 2);
          continue k (ToReplace (v * 2))
        )
      | effect Behead k ->
        (* Behead -> (Yield.Behead?)^* *)
        xs := (!xs |> get).tail;
        continue k ()
  end;
  !xs |> get |> iter1 (fun x ->
    print_endline (string_of_int x)
  )

(*

Output:

$ ./run.sh
+ dune exec ./generator_bi.exe
-1
2
-3
4
5
Beheading -1
Replacing 2 with 4
Beheading -3
Replacing 4 with 8
Replacing 5 with 10
4
8
10

*)
