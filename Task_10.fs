type TimeOfDay = { hours: int; minutes: int; f: string }

let comp = function
 | (x, y) when x.f.Equals("AM") && y.f.Equals("PM") -> false
 | (x, y) when x.f.Equals("PM") && y.f.Equals("AM") -> true
 | (x, y) when x.hours < y.hours -> false
 | (x, y) when x.hours > y.hours -> true
 | (x, y) when x.minutes < y.minutes -> false
 | (x, y) when x.minutes > y.minutes -> true
 | _ -> false

let (.>.) x y = comp(x, y)