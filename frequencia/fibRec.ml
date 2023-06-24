let fib n =
  let ht = Hashtbl.create 99 in
  let counter = ref 0 in
  let rec fibMemo n =
    try Hashtbl.find ht n with
    | Not_found ->
      counter := !counter + 1;
      let result =
        if n <= 1 then 1
        else fibMemo (n - 1) + fibMemo (n - 2)
      in
      Hashtbl.add ht n result;
      !counter
  in
  fibMemo n


let test_fib n =
  let result = fib n in
  Printf.printf "Fibonacci(%d) = %d\n" n result

let () =
  test_fib 10;
  test_fib 20;
  test_fib 30;
