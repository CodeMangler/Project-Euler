#light

(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*)

let divides (n:bigint) (x:bigint) = 
    n % x = 0I;;

let is_prime (n:bigint) =
    let rec prime_check_loop (i:bigint) = 
        ( i > (n / 2I) ) || ((not (i |> divides n)) && prime_check_loop (i + 1I))
    
    prime_check_loop 2I;; // Start checking from 2.. Anything below will be reported as prime, but that should be taken care of by the container..

let rec next_prime (from:bigint) =
    if from < 2I then 2I 
    else if is_prime (from + 1I) then (from + 1I) 
    else next_prime (from + 1I) ;;

// Works, but after 2000, it's horribly slooow
// Managed to calculate only till 20000
let sum_of_primes_below (limit:bigint) = 
    let rec add_prime (n:bigint) (acc:bigint) =
        let next = next_prime n
        if next < limit then
            add_prime next (acc + next)
         else
            acc

    add_prime 0I 0I;;

//Even this has the exact same issues as the previous one.. Horribly slooow..
let prime_series (limit:bigint) =
    let rec prime_series_loop (p:bigint) series =
        if p < limit then
            (prime_series_loop (next_prime p) (p :: series))
        else
            series
    prime_series_loop 2I [];;


// Sieve of Eratosthenes -> http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
// Works like a charm :)     
let prime_series_sieve (limit:bigint) = 
    let series = List.to_array [0I..limit]
    series.SetValue(0I, 1)

    let rec eliminate_multiples (n:bigint) (i:bigint) = 
        let index = (i * n) // Just to reduce as many calculations as possible
        if index < bigint.Parse(series.Length.ToString()) then 
            series.SetValue(0I, (bigint.ToInt64(index)))
            eliminate_multiples n (i + 1I)
    
    for n in [2I..(limit/2I)] do
        eliminate_multiples n 2I
    
    series;;

let sum_of_primes_under (limit:bigint) =
    Array.sum (prime_series_sieve limit);;

// Correct answer: 142913828922I i.e. 142913828922
// sum_of_primes_under 2000000I;; = 142913828922I (and that took about 2.5 minutes to calculate!!!
// Here's my blog post related to this: http://blog.dharampal.name/2008/12/25/sum-of-all-the-primes-below-two-million/ :D
