let rec tribonacci_count n count =
  if n = 0 then (0, count + 1)
  else if n = 1 || n = 2 then (1, count + 1)
  else
    let count1, result1 = tribonacci_count (n - 1) (count + 1) in
    let count2, result2 = tribonacci_count (n - 2) count1 in
    let count3, result3 = tribonacci_count (n - 3) count2 in
    (count3, result1 + result2 + result3)