type 't arvore =
  N of 't arvore * 't * 't arvore * int
  | Folha;;

let altura = function
  | Folha -> 0
  | N (_,_,_,h) -> h

let node l v r = N(l,v,r,1 + max (altura l) (altura r))

(*Função retirada do pdf disponibilizado no Teams*)
let balance l v r =
  let hl = altura l in
  let hr = altura r in
  if hl > hr + 1 then begin (* o problema = à esquerda*)
    match l with
    | N (ll, lv, lr, _) when altura ll >= altura lr ->
    (* caso de uma simples rotação *)
    node ll lv (node lr v r)
    | N (ll, lv, N (lrl, lrv, lrr, _),_) ->
    (* caso precisemos de uma dupla rotação *)
    node (node ll lv lrl) lrv (node lrr v r)
    | _ ->
    (* situação "impossível" mas que temosde considerar *)
    assert false
    end else if hr > hl + 1 then begin (* caso simétrico *)
    match r with
    | N (rl, rv, rr, _) when altura rr >= altura rl ->
      node (node l v rl) rv rr
    | N (N(rll, rlv, rlr, _), rv, rr, _) ->
      node (node l v rll) rlv (node rlr rv rr)
    | _ ->
    assert false
    end else (* caso em que não há rotações por fazer *)
    node l v r

(*Função retirada do pdf disponibilizado no Teams*)
let rec add x = function
  | Folha ->
  N (Folha, x, Folha, 1)
  | N (l, v, r, _) as t ->
  let c = compare x v in
  if c = 0 then t
  else if c < 0 then balance (add x l) v r
  else balance l v (add x r)

(*Guarda o caminho para um elemento pretendido da árvore*)
let rec path t number =
  match t with
  | Folha -> []
  | N (l, v, r, _) ->
    let n = compare v number in 
    if n = 0 then 
      [v] 
    else if n > 0 then 
      v :: path l number 
    else 
      v :: path r number;;



(*search a mutação mais próxima e comum entre dois elementos selecionados*)
let mutation tree number1 number2 =
  if (List.length (path tree number1)) >= (List.length (path tree number2)) then
    let rec aa n =
      if (List.length (path tree number2) - 1 > n) then
        (List.nth (path (tree) number1) n )
      else
        aa n+1 in
    aa 0
  else
    let rec aa n =
      if n = (List.length (path tree number1) - 1) then
        0
      else if List.nth (path (tree) number1) n = List.nth (path (tree) number2) n then
        List.nth (path (tree) number1) n
      else
        aa n+1 in
    aa 0;;

let tree_list = ref [];;

(*search um elemento da árvore e retorna true se existir false se não existir*)
let rec search arvore value =
  match arvore with
  | Folha -> false
  | N (l, v, r, _) ->
    let n = compare value v in 
    if n = 0 then 
      true
    else if n < 0 then 
      (search l value)
    else 
      (search r value);;

let () =
  let forest = read_int() in
  for i = 1 to forest do
    let treesize = read_int () in
    let first = read_int() in
    let tree = ref (node Folha first Folha) in
    for i = 0 to treesize - 2 do
      let number = read_int () in
      tree := add number !tree;
    done;
    tree_list := !tree :: !tree_list;
  done;
 let b,a = Scanf.sscanf (read_line ()) " %d %d" (fun a b -> a,b)  in
 let aux = ref 0 in
 for n = 1 to (List.length !tree_list) do
  if  (search (List.nth !tree_list (List.length !tree_list - n)) a) && (search (List.nth !tree_list (List.length !tree_list - n)) b) then
    let () = aux := mutation (List.nth !tree_list (List.length !tree_list - n)) a b in
    let () = print_int (!aux) in
    print_newline();
  else
    aux := !aux;
 done;
  if !aux = 0 then
    (print_string "NO"; print_newline())
  else 
    ()
