#light

(*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10001st prime number?
*)

let divides n x =
     n % x = 0;;

let rec is_composite seed_divisor n =
    if seed_divisor > (n / 2) then
        false
    else
        if seed_divisor |> divides n then
            true
        else
            n |> is_composite (seed_divisor + 1);;

let is_prime n = 
    not (n |> is_composite 2);;

let rec next_prime from = 
    if is_prime (from + 1) then
        (from + 1)
    else
        next_prime (from + 1);;

let nth_prime n =
    let rec ith_prime seed_prime seed_n = 
        if seed_n = n then
            seed_prime
        else
            ith_prime (next_prime seed_prime) (seed_n + 1)
    
    ith_prime 2 1;;

printf "%d\n" (nth_prime 10001)

// Correct answer: 104743
// nth_prime 10001;; = 104743
