type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let hd (s : 'a cell) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

let tl (s : 'a cell) : Lazy<'a cell> =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g

// 51.3
let rec nth (s : 'a cell) n : 'a = 
  let rec nth_rec (ss: 'a cell) (acc: int): 'a = 
   match ss with
    | ss when acc = n -> hd ss
    | ss -> nth_rec ((tl ss).Force()) (acc+1) 
  nth_rec s 0