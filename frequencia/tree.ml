type 'a bst = E | N of 'a bst * 'a * 'a bst

(* Helper function to insert a value into a BST *)
let rec insert (x : 'a) (tree : 'a bst) : 'a bst =
  match tree with
  | E -> N (E, x, E)  (* Insert at the leaf position *)
  | N (left, value, right) ->
      if x < value then
        N (insert x left, value, right)  (* Recursively insert in the left subtree *)
      else if x > value then
        N (left, value, insert x right)  (* Recursively insert in the right subtree *)
      else
        tree  (* Value already exists, return the original tree *)


(* Função para converter a árvore em uma lista usando a estratégia "em profundidade primeiro, da esquerda para a direita" *)
let rec tolist_dfs (tree : 'a bst) : 'a list =
  match tree with
  | E -> []  (* Caso base: árvore vazia, retorna lista vazia *)
  | N (left, value, right) ->
      let left_list = tolist_dfs left in  (* Recursivamente converter a subárvore esquerda *)
      let right_list = tolist_dfs right in  (* Recursivamente converter a subárvore direita *)
      value :: (left_list @ right_list)  (* Concatenar a lista do nó atual com as listas das subárvores *)

(* Example usage *)
let bst_example : int bst =
  let tree = E in               (* Create an empty tree *)
  let tree = insert 5 tree in   (* Insert value 5 *)
  let tree = insert 3 tree in   (* Insert value 3 *)
  let tree = insert 8 tree in   (* Insert value 8 *)
  let tree = insert 1 tree in   (* Insert value 1 *)
  let tree = insert 4 tree in   (* Insert value 4 *)
  tree

  let values = tolist_dfs bst_example (* Converter a árvore em uma lista *)
  
  let () =
    List.iter (fun x -> print_int x; print_string " ") values;
    print_newline ()

    let tolist_bfs (tree : 'a bst) : 'a list =
      let enqueue queue node = node :: queue in
    
      let rec bfs queue acc =
        match queue with
        | [] -> List.rev acc  (* Return the reversed accumulated list as the result *)
        | E :: rest -> bfs rest acc  (* Skip empty nodes *)
        | N (left, value, right) :: rest ->
            let new_queue = enqueue (enqueue rest left) right in
            bfs new_queue (value :: acc)
    
      in match tree with
      | E -> []  (* Base case: empty tree, return an empty list *)
      | _ -> bfs [tree] []  (* Start BFS traversal with the root node in the queue *)
    
    (* Example usage *)
    let bst_example2 : int bst =
      let tree = E in               (* Create an empty tree *)
      let tree = insert 5 tree in   (* Insert value 5 *)
      let tree = insert 3 tree in   (* Insert value 3 *)
      let tree = insert 8 tree in   (* Insert value 8 *)
      let tree = insert 1 tree in   (* Insert value 1 *)
      let tree = insert 4 tree in   (* Insert value 4 *)
      tree
    
    let values2 = tolist_bfs bst_example2 (* Converter a árvore em uma lista *)
    
    let () =
      List.iter (fun x -> print_int x; print_string " ") values2;
      print_newline ()

    (* Function to count the number of items in a binary tree *)
let rec count_items (tree : 'a bst) : int =
  match tree with
  | E -> 0  (* Base case: empty tree, return 0 *)
  | N (left, _, right) ->
      let left_count = count_items left in  (* Recursively count items in the left subtree *)
      let right_count = count_items right in  (* Recursively count items in the right subtree *)
      1 + left_count + right_count  (* Add 1 for the current node and the counts from the subtrees *)

  let item_count = count_items bst_example  (* Count the items in the binary tree *)

  let () = Printf.printf "%d \n" item_count


  let rec sum_items (tree : 'a bst) : int =
    match tree with
    | E -> 0  (* Base case: empty tree, return 0 *)
    | N (left, value, right) ->
      let left_sum = sum_items left in
      let right_sum = sum_items right in
      value + left_sum + right_sum

  let item_sum = sum_items bst_example  (* Count the items in the binary tree *)

  let () = Printf.printf "%d \n" item_sum

(* Function to find the maximum key value in a binary tree *)
let rec max_key (tree : 'a bst) : int =
  match tree with
  | E -> -1  (* Base case: empty tree, return -1 *)
  | N (_, key, right) ->
      let right_max = max_key right in  (* Recursively find the maximum key in the right subtree *)
      if right_max = -1 then
        key  (* If right subtree is empty, current node has the maximum key *)
      else
        max key right_max  (* Return the maximum of the current key and the maximum from the right subtree *)

  let max_key_value = max_key bst_example

  let () = Printf.printf "%d \n" max_key_value


(* Function to print keys less than a given value in a binary tree *)
let rec print_keys_less_than (tree : 'a bst) (v : 'a) : unit =
  match tree with
  | E -> ()  (* Base case: empty tree, do nothing *)
  | N (left, key, right) ->
      if key < v then begin
        print_int key;
        print_string " ";
        print_keys_less_than left v;  (* Print keys less than v in the left subtree *)
        print_keys_less_than right v  (* Print keys less than v in the right subtree *)
      end
      else if key = v then
        print_keys_less_than left v  (* Print keys less than v in the left subtree *)
      else
        print_keys_less_than left v  (* Print keys less than v in the left subtree *)

(* Example usage *)


let () = print_keys_less_than bst_example 3  (* Print keys less than value in the binary tree *)

(* Function to calculate the height of a binary tree *)
let rec height (tree : 'a bst) : int =
  match tree with
  | E -> 0  (* Base case: empty tree, height is 0 *)
  | N (left, _, right) ->
      let left_height = height left in  (* Calculate height of the left subtree *)
      let right_height = height right in  (* Calculate height of the right subtree *)
      1 + max left_height right_height  (* Height is the maximum height between the left and right subtrees plus 1 *)

  let treeHeight = height bst_example 

  let () = Printf.printf "\n%d \n" treeHeight


(* Function to calculate the cost of the most expensive path from root to leaf in a binary tree *)
let rec max_path_cost (tree : 'a bst) : int =
  match tree with
  | E -> 0  (* Base case: empty tree, cost is 0 *)
  | N (left, key, right) ->
      let left_cost = max_path_cost left in  (* Calculate cost of the most expensive path in the left subtree *)
      let right_cost = max_path_cost right in  (* Calculate cost of the most expensive path in the right subtree *)
      key + max left_cost right_cost  (* Cost is the key of the current node plus the maximum cost from the subtrees *)

  let path_cost = max_path_cost bst_example  (* Calculate the cost of the most expensive path in the binary tree *)

  let () = Printf.printf "%d \n" path_cost

(* Example usage *)
let balanced_tree : int bst =
  N (N (E, 2, E), 1, N (E, 3, E))

let unbalanced_tree : int bst =
  N (E, 1, N (E, 2, N (E, 3, E)))

  (* Function to check if a binary tree is balanced *)
  let rec is_balanced (tree : 'a bst) : bool =
    match tree with
    | E -> true  (* Base case: empty tree, considered balanced *)
    | N (left, _, right) ->
        let left_height = height left in  (* Calculate height of the left subtree *)
        let right_height = height right in  (* Calculate height of the right subtree *)
        let height_diff = abs (left_height - right_height) in  (* Calculate the difference in heights *)
        height_diff <= 1 && is_balanced left && is_balanced right  (* Check if the height difference is at most 1 and both subtrees are balanced *)
  

    let is_balanced1 = is_balanced bst_example  (* Check if the balanced_tree is balanced *)
    let is_balanced2 = is_balanced unbalanced_tree  (* Check if the unbalanced_tree is balanced *)

    let print_bool value =
      if value then
        print_string "true\n"
      else
        print_string "false\n"

    let () = print_bool is_balanced1
    (* let () = print_bool is_balanced2 *)
