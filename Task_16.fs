let rec add_i(st, x) = Set.map (fun el -> Set.add x el) st
 
let rec subset = function
 | 0 -> Set.empty
 | 1 -> set[Set.empty; set[1]]
 | i -> Set.union  (add_i(subset(i - 1), i)) (subset(i - 1))

let rec allSubsets (n: int) (k: int) =
 Set.filter (fun el -> Set.count el = k) (subset(n))