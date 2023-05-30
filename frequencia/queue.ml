type 'a queue = {
  mutable front : int;
  mutable rear : int;
  mutable size : int;
  elements : 'a array;
}

(* Function to initialize a queue *)
let initialize_queue capacity =
  {
    front = 0;
    rear = -1;
    size = 0;
    elements = Array.make capacity (Obj.magic 0);
  }

(* Function to check if the queue is empty *)
let is_empty queue =
  queue.size = 0

(* Function to check if the queue is full *)
let is_full queue =
  queue.size = Array.length queue.elements

(* Function to insert an element into the queue *)
let enqueue queue element =
  if is_full queue then
    failwith "Queue is full"
  else begin
    queue.rear <- (queue.rear + 1) mod (Array.length queue.elements);
    queue.elements.(queue.rear) <- element;
    queue.size <- queue.size + 1
  end

(* Function to remove an element from the queue *)
let dequeue queue =
  if is_empty queue then
    failwith "Queue is empty"
  else begin
    let element = queue.elements.(queue.front) in
    queue.front <- (queue.front + 1) mod (Array.length queue.elements);
    queue.size <- queue.size - 1;
    element
  end

(* Function to display queue elements *)
let display_queue queue =
  if is_empty queue then
    print_endline "Queue is empty"
  else begin
    print_string "Queue elements are: ";
    let index = ref queue.front in
    for _i = 1 to queue.size do
      print_int queue.elements.(!index);
      print_string " ";
      index := (!index + 1) mod (Array.length queue.elements)
    done;
    print_endline ""
  end

(* Example usage *)
let () =
  print_endline "Initialize a queue!";
  let queue = initialize_queue 5 in
  print_endline ("Check the queue is empty or not? " ^ (if is_empty queue then "Yes" else "No"));
  print_endline "";

  print_endline "Insert some elements into the queue:";
  enqueue queue 1;
  enqueue queue 2;
  enqueue queue 3;
  display_queue queue;
  print_endline "";

  print_endline "Insert another element into the queue:";
  enqueue queue 4;
  display_queue queue;
  print_endline "";

  print_endline ("Check the queue is empty or not? " ^ (if is_empty queue then "Yes" else "No"))
