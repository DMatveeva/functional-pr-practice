let f n = 
 let fact = ref 1
 let i = ref 1
 while ! i <= n do
  fact := ! fact * ! i
  i := ! i + 1
 ! fact

// 47.4.2
let fibo n = 
 let fib = ref 0
 let i = ref 0
 while ! i <= n do
  fib := ! fib + ! i
  i := ! i + 1
 ! fib