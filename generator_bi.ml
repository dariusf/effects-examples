
type 'a action
  = ToContinue
  | ToReplace of 'a
  | ToBehead

effect Yield : int -> int action

effect Behead : unit

let yield x = perform (Yield x)

let behead () = perform (Behead)

type 'a lst = {
  mutable head : 'a;
  mutable tail : 'a lst option;
}

let rec iter1 f {head; tail} =
  f head;
  match tail with
  | None -> ()
  | Some t -> iter1 f t

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
      xs.tail <- tl.tail;
      continue k ()

let rec to_lst xs =
  match xs with
  | [] -> None
  | x :: xs1 -> Some { head = x; tail = to_lst xs1 }

let get = Option.get

let () =
  let xs = ref (to_lst [1; 2; 3; 4; 5]) in
  !xs |> get |> iter1 (fun x ->
    print_endline (string_of_int x)
  );
  begin
    try
      iter (!xs |> get)
    with
      | effect (Yield v) k ->
        if v <> 1 && v <> 3 then (
          print_endline ("Replacing " ^ string_of_int v);
          continue k (ToReplace (v * 2))
        ) else (
          print_endline ("Beheading " ^ string_of_int v);
          continue k ToBehead
        )
      | effect Behead k ->
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
1
2
3
4
5
Beheading 1
Replacing 2
Beheading 3
Replacing 4
Replacing 5
4
8
10

*)
