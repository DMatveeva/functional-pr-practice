open System

let rec pow = function
 | (s,1) -> string s
 | (s,n) when n <= 0 -> ""
 | (s,n) -> s + pow(s,n-1)

let rec isIthChar = function
 | (s,n,c) when n < 0 -> false
 | (s,n,c) when n >= String.length s -> false
 | (s,n,c) -> s.[n] = c

let bti = function
 | true -> 1
 | false -> 0

let rec occFromIth_rec = function
 | (s,0,c) -> bti(isIthChar(s,0,c))
 | (s,n,c) -> bti(isIthChar(s,n,c)) + occFromIth_rec(s,n-1,c)

let rec occFromIth = function
 | (s,n,c) when n < 0 -> 0
 | (s,n,c) when n >= String.length s -> 0
 | (s,n,c) -> occFromIth_rec(s,(String.length s)-1,c) - occFromIth_rec(s,n-1,c)