let rec allSubsets n k =
 
 let rec iter = function
   | 0 -> Set.empty 
   | i -> Set.add i (iter(i - 1))
   
 let rec iter_n = function
   | 0 -> Set.empty
   | j -> Set.add (iter j) (iter_n(j - 1))
   
 iter_n n