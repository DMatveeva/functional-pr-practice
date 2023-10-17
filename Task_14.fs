let rec sum (p, xs) = 
  match xs with
   | head :: tail -> if p head = true then head + sum(p, tail) else sum(p, tail) 
   | _ -> 0

let rec count (xs, n) = 
 match xs with
  | head :: tail when head = n -> 1 + count(tail, n)
  | head :: tail when head < n -> count(tail, n)
  | _ -> 0

let rec insert (xs, n: int) = 
 match xs with
  | (head :: tail as ls) -> if head < n then head :: insert(tail, n) else n :: ls
  | [] -> [n]

let rec intersect (xs1: int list, xs2: int list) = 
 match (xs1, xs2) with 
  | ((head1 :: tail1 as ls1), (head2 :: tail2 as ls2)) -> 
   if head1 = head2 then head1 :: intersect(tail1, tail2) 
   elif head1 < head2 then intersect(tail1, ls2)
   else intersect(ls1, tail2)
  |(_, _) -> []

let rec plus (xs1, xs2) = 
 match (xs1, xs2) with
  | ((head1 :: tail1 as ls1), (head2 :: tail2 as ls2)) -> 
   if head1 <= head2 then head1 :: plus(tail1, ls2) 
   else head2 :: plus(ls1, tail2)
  | ([]: int list, []) -> []
  | ([], l2) -> l2
  | (l1, []) -> l1

let rec minus (xs1, xs2) =
 match (xs1, xs2) with
  | ((head1 :: tail1 as ls1), (head2 :: tail2 as ls2)) -> 
   if head1 < head2 then head1 :: minus(tail1, ls2)
   elif head1 = head2 then minus(tail1, tail2)
   else minus(ls1, tail2)
  | (ls1, []: int list) -> ls1
  | ([], _) -> []

let rec smallest xs =
 if xs = [] then None else 
  let rec smallest_rec = function
   | (s: int, [x]) -> if x < s then x else s
   | (s, head :: tail) -> if head < s then smallest_rec(head, tail) else smallest_rec(s, tail)
  let head :: tail = xs
  Some(smallest_rec(head, xs))

let rec delete (n: int, xs) = 
 match xs with 
  | head :: tail -> if head = n then tail else head :: delete(n, tail)
  | [] -> []

let rec sort xs = 
 match xs with 
  | [] -> []
  | xs -> 
   let sm = (Option.get(smallest(xs)))
   sm :: sort(delete(sm, xs))

let rec revrev = function
 | (head: int list :: tail as ls) -> revrev tail @ [List.rev(head)] 
 | [] -> []