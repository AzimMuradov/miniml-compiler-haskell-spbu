let rec g a b = if a = 0 || b = 1 then true else g (a - 1) (b + 5);;

let rec h m p z = let m = p + z in (if m = 1 then h 0 0 z else g m p);;

let g = g 0 2 + (h 2 2 2);;

let g a = a + 1;; let g b = g b + 2;; g 5;;