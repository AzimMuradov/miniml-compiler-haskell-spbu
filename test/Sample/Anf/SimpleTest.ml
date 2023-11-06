let rec g x = if x < 0 then 0 else g (x + 1)

let g x = g (-5) + x;;

g 3;;