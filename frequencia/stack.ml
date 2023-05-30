type 'a stack = {
  mutable elements: 'a array;
  mutable top: int;
}

(* Function to initialize a stack *)
let initialize_stack size =
  {
    elements = Array.make size None;
    top = -1;
  }

(* Function to check if the stack is empty *)
let is_empty stack =
  stack.top = -1

(* Function to check if the stack is full *)
let is_full stack =
  stack.top = Array.length stack.elements - 1

(* Function to push an element onto the stack *)
let push stack element =
  if is_full stack then
    failwith "Stack is full"
  else
    begin
      stack.top <- stack.top + 1;
      stack.elements.(stack.top) <- Some element
    end

(* Function to pop an element from the stack *)
let pop stack =
  if is_empty stack then
    failwith "Stack is empty"
  else
    begin
      let element = stack.elements.(stack.top) in
      stack.top <- stack.top - 1;
      Option.get element
    end

(* Function to display stack elements *)
let display_stack stack =
  if is_empty stack then
    print_endline "Stack is empty"
  else
    begin
      print_string "Elements in the stack are: ";
      for i = 0 to stack.top do
        print_int (Option.get stack.elements.(i));
        print_string " "
      done;
      print_endline ""
    end

(* Example usage *)
let () =
  let stack = initialize_stack 6 in
  push stack 3;
  push stack 5;
  push stack 4;
  push stack 3;
  push stack 2;
  push stack 1;
  display_stack stack;
  pop stack;
  pop stack;
  pop stack;
  display_stack stack;
