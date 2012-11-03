#light

(*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
Find the largest palindrome made from the product of two 3-digit numbers.
*)

// ALL BADLY NAMED!!! REFACTOR!!!

// Crude. But is there a better way?
let is_palindrome n = 
    let n_as_array = n.ToString().ToCharArray()
    let reverse_n = Array.rev n_as_array
    reverse_n = n_as_array;;

let rec first_palindromic_product_of (n:int) (i:int) =
    if(is_palindrome (n * i)) then
        n * i
    else
        first_palindromic_product_of n (i - 1);;

let palindrome_made_of n =
    first_palindromic_product_of n 999;;
    
let rec palindromic_multiple n acc = 
    if n = 0 then
        acc
    else
        palindromic_multiple (n - 1) ((palindrome_made_of n) :: acc) ;;
        
let largest_three_digit_product_palindrome =
    List.max (palindromic_multiple 999 []) ;;

printf "%d\n" largest_three_digit_product_palindrome

// Correct answer: 906609
// i.e. largest_three_digit_product_palindrome;; = 906609
