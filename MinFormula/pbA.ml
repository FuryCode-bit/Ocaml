open F_parser

let input_formula = parse "stdin"

(*finds the smallest lexicographically word *)
let formula_min_var form =
  let rec aux f =
    match f with
    | True | False -> "Z"
    | Var a -> a
    | Not a -> aux a
    | And (a,b) | Or (a,b) | Implies (a,b) | Equiv (a,b) ->
        let a_min = aux a in
        let b_min = aux b in
        if a_min < b_min then a_min else b_min
  in aux form

(* Decodes a formula using all the operators to NOR operator *)
let rec formula_decoder opt minimal =
  match opt with
  | False -> sprintf "(%s %% (%s %% %s))" minimal minimal minimal
  | True -> formula_decoder (Not False) minimal
  | Var a -> a
  | Not (Or (a, b)) -> sprintf "(%s %% %s)" (formula_decoder a minimal) (formula_decoder b minimal)
  | Not a -> sprintf "(%s %% %s)" (formula_decoder a minimal) (formula_decoder a minimal)
  | And (a, b) -> sprintf "((%s %% %s) %% (%s %% %s))" (formula_decoder a minimal) (formula_decoder a minimal) (formula_decoder b minimal) (formula_decoder b minimal)
  | Or (a, b) -> sprintf "((%s %% %s) %% (%s %% %s))" (formula_decoder a minimal) (formula_decoder b minimal) (formula_decoder a minimal) (formula_decoder b minimal)
  | Implies (a, b) -> formula_decoder (Or (Not a, b)) minimal
  | Equiv (a, b) -> formula_decoder (And (Implies (a, b), Implies (b, a))) minimal

(* Converts the type of the given list to a proper list *)
let rec to_list x =
  match x with
  | Some e -> e
  | None -> []

(* Decodes each formula in a list *)
let decode_lst lst =
  List.map (fun x -> formula_decoder x (formula_min_var x)) lst

(* Output *)
let () =
  let lst = decode_lst (to_list input_formula) in
  List.iter (fun x -> Printf.printf "%s\n" x) lst
