let try_find key m = 
 let xs = Map.toList m
 let rec f lst =
  match lst with
   | [] -> None
   | (x1, x2) :: tail -> if x1 = key then Some(x2) else f(tail)
 f xs