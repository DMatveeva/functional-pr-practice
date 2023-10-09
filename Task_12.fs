let rec upto n = 
 let rec upto_rec = function
  | m when m = n -> [n] 
  | m -> m :: upto_rec(m + 1)
 upto_rec 1

let rec dnto = function
 | n when n < 1 -> []
 | 1 -> 1 :: []
 | n -> n :: upto(n - 1)

let rec evenn n = 
 let rec evenn_rec = function
  | m when m = (n - 1) * 2 -> [m]
  | m -> m :: evenn_rec(m + 2)
 evenn_rec 0