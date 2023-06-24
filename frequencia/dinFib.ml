let dynamicFib n = 
  let arr = Array.make (n+1) 0 in
  arr.(0) <- 1;
  arr.(1) <- 1;
  if n >= 1 then arr.(1) <- 1;
  for i = 2 to n do
    arr.(i) <- 1+ arr.(i-1) + arr.(i-2)
  done;
  
  arr.(n);;

  let test_dynamicFib n =
    let result = dynamicFib n in
    Printf.printf "DinFibonacci(%d) = %d\n" n result
  ;;
  
  test_dynamicFib 10;
  test_dynamicFib 20;
  test_dynamicFib 30