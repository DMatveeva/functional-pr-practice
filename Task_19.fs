let rec fibo1 n n1 n2 = 
 let rec f x x1 x2 = 
  if x = 1 then x1
  else f (x-1) (x1 + x2) (x1)
 f n n1 n2 

let rec fibo2 n c = 
 if n = 1 then c 1
 else fibo2 (n-1) (fun x -> c(x + n))

let rec bigList n  k =
 if n=0 then k []
 else bigList (n-1) (fun res -> k(1::res))