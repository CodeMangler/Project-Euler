#light

(*
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*)

//Lifted from Expert F#.. Should write my own..
let is_prime (n:bigint) =
    let half_n:bigint = n / 2I // I (Caps I) -> BigInt
    let rec check_divisibility x = 
        x > half_n || ((n % x <> 0I) && check_divisibility (x + 1I) )
    check_divisibility 2I ;;

let rec prime_factors (n:bigint) (factors:list<bigint>) =
    if (is_prime n) then
        n :: factors
    else
        let mutable i:bigint = 2I;
        while (n % i <> bigint.Zero) && (i <= n/2I) do
            i <- i + 1I
        done
        
        if( i <= n/2I) then
            factors @ (prime_factors i factors) @ (prime_factors (n / i) factors)
        else
            []
;;

printf "%A\n" (prime_factors 600851475143I [])

// Correct answer: 6857
// i.e. prime_factors 600851475143I [];; = [71I; 839I; 1471I; 6857I]
// List.max (prime_factors 600851475143I []);; = 6857I
