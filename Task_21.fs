let rec fact = function
 | 0 -> 1
 | n -> n * fact(n - 1)

let fac_seq n = seq {
 for i in 1..n do
  yield fact i
}

let seq_seq n = seq {
 for i in 0..n do
  if i = 0 then 
   yield 0
  else 
   yield -i
   yield i  
}