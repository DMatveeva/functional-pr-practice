let even_seq = Seq.initInfinite (fun i -> 2 * (i+1))

let rec fact = function
 | 0 -> 1
 | n -> n * fact(n - 1)

let fac_seq = Seq.initInfinite (fun i ->  fact i)

let c = function 
 | 0 -> 0
 | n when n % 2 = 1 -> -(n / 2) - 1
 | n -> n / 2 

let seq_seq = Seq.initInfinite (fun i -> c i)