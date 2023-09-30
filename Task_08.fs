let curry (f: int * int -> int)  = 
 let g x =
  let h y =
   f(x,y)
  h
 g

let uncurry g =
 let f(x: int, y: int) : int =
   g x y
 f
