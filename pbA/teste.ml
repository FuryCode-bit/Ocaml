type variable = string

type formula =
  | Var of variable
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Equiv of formula * formula
  | True
  | False

let rec minimal_formula var_count = function
  | Var v -> Var v
  | Not f -> Or (minimal_formula var_count f, minimal_formula var_count f)
  | And (f1, f2) ->
      Or (Or (minimal_formula var_count f1, minimal_formula var_count f2),
          Or (Not (minimal_formula var_count f1), Not (minimal_formula var_count f2)))
  | Or (f1, f2) ->
      And (And (minimal_formula var_count f1, minimal_formula var_count f2),
           Or (Not (minimal_formula var_count f1), Not (minimal_formula var_count f2)))
  | Implies (f1, f2) ->
      minimal_formula var_count (Or (Not f1, f2))
  | Equiv (f1, f2) ->
      minimal_formula var_count (And (Implies (f1, f2), Implies (f2, f1)))
  | True -> Not (minimal_formula var_count False)
  | False -> Var (String.make 1 (Char.chr (var_count + 90)))

let to_minimal_formula s =
  match parse s with
  | Some [f] -> minimal_formula 0 f |> to_string
  | _ -> failwith "invalid input"

let () =
  match read_line () with
  | s -> print_endline (to_minimal_formula s)

