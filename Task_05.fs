// 16.1
let notDivisible (n,m) = match n with
    | 0 -> false
    | n -> m - int(m / n) * n = 0   

// 16.2
let prime n = 
    let rec notPrimeRec = function
        | 1 -> false
        | d -> notDivisible(d, n) || notPrimeRec(d-1)
    let k = n / 2
    not (notPrimeRec k)
