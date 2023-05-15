(* This function implements the 0-1 Knapsack problem using dynamic programming *)
let knapsack n prices =
  (* Create an array mem of size n+1 initialized to zeros *)
  let mem = Array.make (n+1) 0 in
  (* Define a recursive function loop that processes each item in the prices list *)
  let rec loop = function
    (* If the list is empty, do nothing *)
    | [] -> ()
    (* Otherwise, take the first item and iterate over all possible sizes from size to n *)
    | (size, price) :: rest ->
        for i = size to n do
          (* Compute the maximum value that can be obtained by either skipping the current item or including it *)
          mem.(i) <- max mem.(i) (mem.(i-size) + price)
        done;
        (* Recursively call loop with the remaining items *)
        loop rest
  in
  (* Call loop with the prices list and return the maximum value that can be obtained with a knapsack of size n *)
  loop prices;
  mem.(n)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

(* This is the main function that reads input, calls the knapsack function, and prints the result *)
let () =
  (* Read the knapsack size limit n and the number of available items m from standard input *)
  let n = read_int () in
  let m = read_int () in

  (* Read the sizes and values of the available items from standard input and store them in a reference prices *)
  let prices = ref [] in
  for i = 1 to m do
    let size, price = Scanf.scanf "%d %d\n" (fun size price -> size, price) in
    prices := (size, price) :: !prices
  done;

  (* Call the knapsack function with n and the list of item sizes and values, and store the result in profit *)
  let profit = time knapsack n !prices in

  (* Print the maximum profit that can be obtained with a knapsack of size n to standard output *)
  print_int profit;
  print_newline ();
