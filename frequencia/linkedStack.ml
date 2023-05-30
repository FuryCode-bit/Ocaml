type 'a node = {
  value: 'a;
  mutable next: 'a node option;
}

type 'a stack = {
  mutable top: 'a node option;
}

(* Function to initialize a stack *)
let initialize_stack () =
  {
    top = None;
  }

(* Function to check if the stack is empty *)
let is_empty stack =
  stack.top = None

(* Function to push an element onto the stack *)
let push stack element =
  let new_node = { value = element; next = stack.top } in
  stack.top <- Some new_node

(* Function to pop an element from the stack *)
let pop stack =
  match stack.top with
  | None -> failwith "Stack is empty"
  | Some top_node ->
    let removed_element = top_node.value in
    stack.top <- top_node.next;
    removed_element

(* Function to display stack elements *)
let display_stack stack =
  let rec print_elements node =
    match node with
    | None -> ()
    | Some n ->
      print_int n.value;
      print_string " ";
      print_elements n.next
  in
  match stack.top with
  | None -> print_endline "Stack is empty"
  | Some _ ->
    print_string "Elements in the stack are: ";
    print_elements stack.top;
    print_endline ""

(* Example usage *)
let () =
  let stack = initialize_stack () in
  push stack 1;
  push stack 2;
  push stack 3;
  push stack 4;
  print_endline "Push data 1 2 3 4";
  display_stack stack;
  print_endline "";
  print_endline "Pop data:";
  while not (is_empty stack) do
    let popped_data = pop stack in
    print_endline ("Pop data: " ^ string_of_int popped_data)
  done;
  print_endline "";
  print_endline "Check a stack is empty or not?";
  print_endline ("Stack is " ^ (if is_empty stack then "empty!" else "not empty!"));
