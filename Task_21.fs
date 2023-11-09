let rec fact = function
 | 0 -> 1
 | n -> n * fact(n - 1)

let fac_seq =
 let rec numbers n = seq {
  yield fact n
  yield! numbers (n+1)
 }
 numbers 0 


let seq_seq =
 let rec numbers n = seq { 
  if n = 0 then 
   yield 0
  else 
   yield -n
   yield n  
  yield! numbers (n+1) 
 }
 numbers 0