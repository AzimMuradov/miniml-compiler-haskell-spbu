let id x = x

let fib n =
  let rec fib_cps n k =
    if n < 3
      then k 1
      else fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
  in fib_cps n id;;

print_int (fib 10)
