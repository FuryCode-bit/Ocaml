type 'a node = {
  value: 'a;
  mutable next: 'a node option;
}

type 'a queue = {
  mutable front: 'a node option;
  mutable rear: 'a node option;
}

(* Function to initialize a queue *)
let initialize_queue () =
  {
    front = None;
    rear = None;
  }

(* Function to check if the queue is empty *)
let is_empty queue =
  queue.front = None

(* Function to insert an element into the queue *)
let enqueue queue element =
  let new_node = { value = element; next = None } in
  match queue.rear with
  | None ->
    queue.front <- Some new_node;
    queue.rear <- Some new_node
  | Some rear_node ->
    rear_node.next <- Some new_node;
    queue.rear <- Some new_node

(* Function to remove an element from the queue *)
let dequeue queue =
  match queue.front with
  | None -> failwith "Queue is empty"
  | Some front_node ->
    let removed_element = front_node.value in
    queue.front <- front_node.next;
    if queue.front = None then queue.rear <- None;
    removed_element

(* Function to count the number of elements in the queue *)
let count_elements queue =
  let count = ref 0 in
  let rec count_helper node =
    match node with
    | None -> !count
    | Some n ->
      count := !count + 1;
      count_helper n.next
  in
  count_helper queue.front

(* Function to display queue elements *)
let display_queue queue =
  let rec print_elements node =
    match node with
    | None -> ()
    | Some n ->
      print_int n.value;
      print_string " ";
      print_elements n.next
  in
  match queue.front with
  | None -> print_endline "Queue is empty"
  | Some _ ->
    print_string "Queue elements are: ";
    print_elements queue.front;
    print_endline ""

(* Reverses the elements of a queue *)
let reverse_queue queue =
  let rec reverse_helper prev current =
    match current with
    | None -> prev
    | Some node ->
      let next_node = node.next in
      node.next <- prev;
      reverse_helper (Some node) next_node
  in
  queue.front <- reverse_helper None queue.front;
  (* Rear needs to be updated to the last node after reversal *)
  let rec update_rear node =
    match node with
    | None -> ()
    | Some n ->
      if n.next = None then
        queue.rear <- Some n
      else
        update_rear n.next
  in
  update_rear queue.front

(* Calculates the sum of the elements in a queue *)
let sum_of_elements queue =
  let sum = ref 0 in
  let rec sum_helper node =
    match node with
    | None -> !sum
    | Some n ->
      sum := !sum + n.value;
      sum_helper n.next
  in
  sum_helper queue.front

  (* Computes the average value of the elements in a queue *)
let average_of_elements queue =
  let sum = sum_of_elements queue in
  let count = count_elements queue in
  if count = 0 then
    0.0
  else
    float_of_int sum /. float_of_int count

    (* Finds the maximum element in a queue *)
let find_maximum queue =
  match queue.front with
  | None -> failwith "Queue is empty"
  | Some front_node ->
    let max_value = ref front_node.value in
    let rec find_max_helper node =
      match node with
      | None -> !max_value
      | Some n ->
        if n.value > !max_value then
          max_value := n.value;
        find_max_helper n.next
    in
    find_max_helper queue.front

    (* Finds the minimum element in a queue *)
let find_minimum queue =
  match queue.front with
  | None -> failwith "Queue is empty"
  | Some front_node ->
    let min_value = ref front_node.value in
    let rec find_min_helper node =
      match node with
      | None -> !min_value
      | Some n ->
        if n.value < !min_value then
          min_value := n.value;
        find_min_helper n.next
    in
    find_min_helper queue.front

    (* Deletes the nth element of a queue *)
let delete_nth_element queue n =
  if n <= 0 then
    failwith "Invalid index"
  else if n = 1 then (
    match queue.front with
    | None -> failwith "Queue is empty"
    | Some front_node ->
      queue.front <- front_node.next;
      if queue.front = None then queue.rear <- None;
  )
  else (
    let rec delete_helper node count =
      match node with
      | None -> ()
      | Some n ->
        if count = n - 1 then (
          match n.next with
          | None -> failwith "Index out of bounds"
          | Some next_node ->
            n.next <- next_node.next;
            if n.next = None then queue.rear <- Some n;
        )
        else
          delete_helper n.next (count + 1)
    in
    delete_helper queue.front 1
  )

(* Example usage *)
let () =
  print_endline "Initialize a queue!";
  let queue = initialize_queue () in
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
  
  let max_value = find_maximum queue in
  print_endline ("Maximum element in the queue: " ^ string_of_int max_value);

  let min_value = find_minimum queue in
  print_endline ("Minimum element in the queue: " ^ string_of_int min_value);

  let numElements = count_elements queue in

  print_endline ("Number of elements in the queue: " ^ string_of_int numElements);

  print_endline ("Check the queue is empty or not? " ^ (if is_empty queue then "Yes" else "No"));

  reverse_queue queue;
  print_endline "Queue elements after reversal:";
  display_queue queue;

  print_endline "Remove an element from the queue:";
  let removed_element = dequeue queue in
  print_endline ("Removed element: " ^ string_of_int removed_element);
  display_queue queue;

  let sum = sum_of_elements queue in
  print_endline ("Sum of elements in the queue: " ^ string_of_int sum);

  let numElements2 = count_elements queue in
  print_endline ("Number of elements in the queue: " ^ string_of_int numElements2)

  let average = average_of_elements queue in
  print_endline ("Average of elements in the queue: " ^ string_of_float average);