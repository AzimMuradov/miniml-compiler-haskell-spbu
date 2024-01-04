let id x = x

let rec cps_factorial n k =
  if n = 0 then
    k 1
  else
    cps_factorial (n - 1) (fun result -> k (n * result))

let factorial n = cps_factorial n id;;

print_int (factorial 5)
