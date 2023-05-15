let knapsack w items =
  let max_value = ref 0 in
  let sum_objects = ref 0 in
  let remaining_capacity = ref w in
  let rec pick_item items =
    if !remaining_capacity = 0 then ()
    else
      match items with
      | [] -> ()
      | (weight, value) :: xs ->
        if !remaining_capacity >= weight then (
          remaining_capacity := !remaining_capacity - weight;
          max_value := !max_value + value;
          sum_objects := !sum_objects + 1;
          pick_item items
        )
        else pick_item xs
  in
  let sorted_items = List.sort (fun (w1, v1) (w2, v2) ->
    let ratio1 = float_of_int w1 /. float_of_int v1 in
    let ratio2 = float_of_int w2 /. float_of_int v2 in
    match compare ratio1 ratio2 with
    | 0 -> compare v2 v1
    | c -> c
  ) items in
  let () = List.iter (fun (w, v) -> Printf.printf "(%d,%d)" w v) sorted_items in
  let () = Printf.printf "\n" in
  pick_item sorted_items;
  (!max_value, !sum_objects)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let () =
  let w = ref 0 in
  let n = ref 0 in
  let items = ref [] in
  Scanf.scanf "%d\n%d\n" (fun x y -> w := x; n := y);
  for i = 1 to !n do
    let (weight, value) = Scanf.scanf "%d %d\n" (fun x y -> (x, y)) in
    items := (weight, value) :: !items;
  done;
  let result = time knapsack !w !items in
  Printf.printf "%d\n" (fst result);
