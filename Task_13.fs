let rec rmodd = function
 | [] -> []
 | [x: int] -> []
 | head :: head2 :: tail -> head2 :: rmodd tail

// 39.2
let rec del_even = function 
 | [] -> []
 | head :: tail -> if head % 2 = 1 then head :: del_even tail else del_even tail

// 39.3
let rec multiplicity (x: int) (xs: int list) =
 let rec mult_rec = function
  | [] -> 0
  | head :: tail -> if head = x then 1 + mult_rec tail else mult_rec tail
 mult_rec xs

// 39.4
let rec split xs =
 let rec split_rec = function
  | ([]: int list, ev, unev) -> (ev, unev)
  | ([x], ev, unev) -> (ev @ [x], unev)
  | (head :: head2 :: tail, ev, unev) -> split_rec(tail, ev @ [head], unev @ [head2])
 split_rec(xs, [], [])

// 39.5
let rec zip = function
 | ([] : int list, []: int list) -> []
 | ([], xs2) -> raise (System.ArgumentException("Lists lenght differs."))
 | (x1, []) -> raise (System.ArgumentException("Lists lenght differs."))
 | (head1 :: tail1, head2 :: tail2) -> (head1, head2) :: zip (tail1, tail2)
