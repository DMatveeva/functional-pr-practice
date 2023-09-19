open System

let rec pow = function
 | (s,1) -> string s
 | (s,n) -> s + pow(s,n-1)

let rec isIthChar(s: string,n,c) =
 s.[n] = c

let bti = function
 | true -> 1
 | false -> 0

let rec occFromIthRec = function
 | (s,0,c) -> bti(isIthChar(s,0,c))
 | (s,n,c) -> bti(isIthChar(s,n,c)) + occFromIthRec(s,n-1,c)

let rec occFromIth(s,n,c) = 
 let last = String.length s - 1
 let sn = s[n..last]
 let len_s = String.length sn - 1
 occFromIthRec(s[n..last],len_s,c)