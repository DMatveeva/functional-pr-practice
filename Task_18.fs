let f n = 
 let fact = ref 1
 let i = ref 1
 while ! i <= n do
  fact := ! fact * ! i
  i := ! i + 1
 ! fact

// 47.4.2
let fibo n = 
 let fib0 = ref 0
 let fib1 = ref 1
 let fib = ref 0
 let i = ref 2
 if n > 1 then 
  while ! i <= n do
   fib := ! fib0 + ! fib1
   fib0 := ! fib1
   fib1 := ! fib
   i := ! i + 1
  ! fib 
 elif n = 0 then 0
 else 1
