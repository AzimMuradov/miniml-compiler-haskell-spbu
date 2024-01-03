let factorial n =
  let rec loop i accum =
    if i > n
      then accum
      else loop (i + 1) (accum * i)
  in loop 1 1;;

print_int (factorial 5)
