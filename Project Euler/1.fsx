#light

// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.

// NOT REQUIRED
// let unique list = [for x in (Set.of_list list) -> x];;

let rec sum list =
    match list with
    | [] -> 0
    | _ -> (List.hd list) + sum (List.tl list) ;;
    

let multiple_of x n = (x % n = 0) ;;

// OR a simpler sum:
// let sum list = List.fold_left (+) 0 list

// A = {3, 6, 9 ...}
// B = {5, 10, 9 ...}
// x ∈ A and x ∈ B
let numbers = List.filter (fun x -> ((multiple_of x 3) || (multiple_of x 5))) [1..999];; // aw! fuck! I was including 1000.. that's why the extra 1000!
printfn "%d" (numbers |> sum) ;;

// Correct answer: 233168 -> http://timjoh.com/project-euler-1-sum-of-the-multiples-of-3-and-5/
// And I keep on getting 234168
// Where the fuck is the additional 1000 coming from?!

// The python equivalent -> sum([x for x in range(1,1000) if x % 3 == 0 or x % 5 == 0])

// btw, there're MUCH better solutions (which don't ACTUALLY sum and calculate the whole thing with a formula :D )
