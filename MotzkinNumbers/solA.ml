let memoize f =
  let value = Hashtbl.create 10 in
  fun x ->
    try Hashtbl.find value x
    with Not_found ->
      let result = f x in
      Hashtbl.add value x result;
      result

let rec mtz n =
  if n = 0 || n = 1 then Z.one
  else
    let num =
      Z.add
        (Z.mul (Z.of_int ((2 * n) + 1)) (mtz (n - 1)))
        (Z.mul (Z.of_int ((3 * n) - 3)) (mtz (n - 2)))
    and den = Z.of_int (n + 2) in
    Z.div num den

let memo_mtz = memoize mtz

let () = read_int () |> memo_mtz |> Z.to_string |> print_endline 

(* ---------------------------------------------------------------------------------------------------------------------------*)

(* Solução sem a Library Z *)

let memoize1 f =
  let value = Hashtbl.create 10 in
  fun x ->
    try Hashtbl.find value x
    with Not_found ->
      let result = f x in
      Hashtbl.add value x result;
      result

let rec mtz1 n =
  if n = 0 || n = 1 then 1
  else
    let num =
      (((2*n + 1) * mtz1(n-1)) + ((3*n - 3) * mtz1(n-2))) in
    num/(n+2)


let memo_mtz1 = memoize1 mtz1

let () = read_int () |> memo_mtz1 |> string_of_int |> print_endline
