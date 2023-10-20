let list_filter (f: int -> bool) xs: int list = List.foldBack (fun head tail -> if f head = true then head :: tail else tail) xs []

let sum (p, xs) = 
 let head :: tail = xs
 let f = fun x y -> 
  if p y = true then x + y else x
 List.fold f 0 xs

let rev lst = List.fold (fun head tail -> tail::head) [] lst

let revrev xs = 
 let head :: tail = xs
 let f = fun head tail -> rev tail :: head
 List.fold f [] xs