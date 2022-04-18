(* Author: Marco Bernardes*)

let count = ref 0

let assist =  ref 0
let rec schroder n = 
  (* Calculo de Sn através da primeira fórmula *)
  assist := 0;
  count := !count + 1;
  match n with
  | 0 -> 1
  | 1 -> 2
  | _ -> begin
            for i = 1 to (n-2) do
              assist:= schroder(i)*schroder(n-i-1) + !assist
            done;
            3*schroder(n-1) + !assist
          end;;

let rec schroder2 n =
  (* Calculo de Sn através da segunda fórmula *)
  count:= !count + 1;
  match n with
    | 0 -> 1
    | 1 -> 2
    | _ -> ((6*n-3)*(schroder2 (n-1)))/(n+1)-(((n-2)*(schroder2 (n-2)))/(n+1))
  
let lista = ref [Z.of_int 1; Z.of_int 2]

open Z
open Scanf
open Printf

let a,b  = Scanf.sscanf (read_line ()) " %d %d" (fun a b -> a,b)
let () =
    (* Calcula o resultado de Sb *)
    for i = 0 to b do
      lista := ( ((Z.of_int 6 * Z.of_int i - Z.of_int 3) * (List.nth !lista 0)) - ((Z.of_int i - Z.of_int 2) * (List.nth !lista 1) ) ) / (Z.of_int i + Z.of_int 1) :: !lista;
    done;



    Printf.printf "%d " (schroder(a));
    Printf.printf "%d\n" (!count);
    count := 0;

    Printf.printf "%d " (schroder2(a));
    Printf.printf "%d\n" (!count);

    print_string ( Z.to_string ( List.hd !lista ) );
    print_newline ();

(* Alguns exemplos
*  Input: 6 100
*  Outupt: 8558 169
*          8558 41
*          28747611153504860266534250007458881388313583561117443629896620307440340890
*
*  Input: 7 200
*  Outupt: 1806 70
*          1806 25
*          367003639688164082769773967887152122611416949480351485551947010043573745553126664892688140481889446267272033254682866545276795378325338214680123861234
*
*  Input: 8 100
*  Outupt: 41586 408
*          41586 67
*          28747611153504860266534250007458881388313583561117443629896620307440340890
*)