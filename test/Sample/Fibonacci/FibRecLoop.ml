let fib n =
  let rec loop i a b =
    if i = 0
      then a
      else loop (i - 1) b (a + b)
  in loop n 0 1;;

print_int (fib 10)
