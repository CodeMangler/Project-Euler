#light

(*
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

// a + b + c = 1000 => c = 1000 - a - b
// a^2 + b^2 = c^2 => a^2 + b^2 = ( 1000 - a - b )^2
// a^2 + b^2 = (1000 ^ 2) - 1000 a - 1000b - 1000a + (a^2) + ab - 1000b + ab + (b^2)
// a^2 + b^2 = a^2 + b^2 + 1000^2 - 2000 a - 2000b + 2ab
// 0 = 1000^2 - 2000a - 2000b + 2ab
// 1000 ( a + b ) - ab = (1000^2)/2 = 500000
// ??!

let sqr x = x * x ;;

let is_pythagorean_triplet a b c =
    sqr a + sqr b = sqr c ;;

let rec calculate_pythagorean_triplet_with_sum sum a b = 
    let c = sum - a - b

    if ( (a + b + c) > (sum * 2) ) then
        printfn "Please check your inputs" // Just a safety check to avoid an infinite loop
        [-1; -1; -1]
    else
        if is_pythagorean_triplet a b c then
            [a; b; c]
        else
            if b = sum then
                calculate_pythagorean_triplet_with_sum sum (a + 1) 1
            else
                calculate_pythagorean_triplet_with_sum sum a (b + 1) ;;

let pythagorean_triplet_with_sum sum = 
    calculate_pythagorean_triplet_with_sum sum 1 1;;

let solution =
    let triplet = pythagorean_triplet_with_sum 1000
    (triplet.[0] * triplet.[1] * triplet.[2]);;

printf "%d\n" solution

// Correct answer: 31875000
// pythagorean_triplet_with_sum 1000;; = [200; 375; 425]
// solution ;; = 31875000
