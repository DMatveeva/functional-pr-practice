let vat (n: int) (x: float) = x * float (100 + n) / 100.0

// 20.3.2
let unvat (n: int) (x: float) = x * 100.0 / float ( 100 + n ) 

// 20.3.3
let rec min ( f: int -> int) = 
 let rec f2 = function
  | 2147483647 -> 2147483647
  | n -> if f n = 0 then n else f2(n+1)
 f2 1