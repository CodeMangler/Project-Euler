#light

(*
The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)

let sum_of_first_n n =
    n * (n + 1)/2;;
    
let sum_of_squares_of_first_n n =
    n * (n + 1) * (2 * n + 1)/6;;

let square x = x * x;;

let solution = 
    abs ((sum_of_squares_of_first_n 100) - (square (sum_of_first_n 100)));;

printf "%d\n" solution

// Correct answer: 25164150
// solution ;; = 25164150
