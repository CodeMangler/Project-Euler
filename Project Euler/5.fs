#light

(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
*)

let divisible n x = 
    n % x = 0 ;;

let rec divisible_by_all_not_exceeding n x = 
    if x = 1 then
        true
    else
        if divisible n x then
            divisible_by_all_not_exceeding n (x - 1)
        else
            false;;


let rec number_divisible_by_all_numbers_not_exceeding n iteration = 
    if divisible_by_all_not_exceeding (n * iteration) n then
        n * iteration
    else
        number_divisible_by_all_numbers_not_exceeding n (iteration + 1);;

let smallest_number_divisible_by_all_numbers_not_exceeding_20 = 
    number_divisible_by_all_numbers_not_exceeding 20 1;;

// Correct answer: 232792560
// smallest_number_divisible_by_all_numbers_not_exceeding_20 ;; -> 232792560
