(*
The following iterative sequence is defined for the set of positive integers:

n  n/2 (n is even)
n  3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13  40  20  10  5  16  8  4  2  1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*)

#light

let even (n:bigint) = (n % 2I = 0I)

let hailstoneSequence (start:int) =
  let rec _hailstoneSequence (n:bigint) sequence =
    match n with
      | n when n = 1I -> sequence @ [n]
      | _ when even(n) -> _hailstoneSequence (n/2I) (sequence @ [n])
      | _ -> _hailstoneSequence ((n*3I) + 1I) (sequence @ [n])
  _hailstoneSequence (bigint start) []

let longestHailstoneSequence limit =
  let rec _longestSequence n limit candidate candidateLength =
    match n with
      | _ when n <= limit ->
        let sequenceLength = (hailstoneSequence n).Length
        if sequenceLength > candidateLength then _longestSequence (n+1) limit n sequenceLength
        else _longestSequence (n+1) limit candidate candidateLength
      | _ -> (candidate, candidateLength)
      
  _longestSequence 1 limit 1 1

// Takes about 28 minutes for a million on an MBP i7 2.4GHz. Uses only one core.
let bruteforceSolution limit =
  let number, sequenceLength = longestHailstoneSequence limit
  printf "Number: %d, Sequence Length: %d" number sequenceLength

// bruteforceSolution 1000000

// Answer: 837799 (Sequence Length: 525)

// Interesting: Using regular ints instead of a bigint (or at least int64) for calculations causes the program to freeze at 113383
// Apparently, it produces a sequence of 248 numbers, one of which (2482111348) is larger than the int range
// While this should've caused the program to fail with an exception or something, it just freezes instead (not sure where)

// Memoized version
open System.Collections.Generic

let hailstoneSequences limit =  
  let rec _hailstoneSequence (n:bigint) sequence (sequences:Dictionary<bigint, bigint list>) =
    match n with
      | _ when n = 1I -> sequence @ [n]
      | _ when sequences.ContainsKey(n) -> sequence @ sequences.[n]
      | _ when even(n) -> _hailstoneSequence (n/2I) (sequence @ [n]) sequences
      | _ -> _hailstoneSequence ((n*3I) + 1I) (sequence @ [n]) sequences

  let rec _nextSequence n limit (sequences:Dictionary<bigint, bigint list>) =
    match n with
      | n when n <= limit -> 
        let sequence = _hailstoneSequence n [] sequences
        sequences.Add(n, sequence)
        _nextSequence (n+1I) limit sequences
      | _ -> sequences

  _nextSequence 1I limit (Dictionary<bigint, bigint list>())

let longestSequence sequences =
  Seq.maxBy (fun (entry:KeyValuePair<bigint, bigint list>) -> entry.Value.Length) sequences

// Takes about 26 seconds on an MBP i7, 2.4GHz. :) Single core again.
// Can be optimized further, but, this works well enough :)
let memoizedSolution limit =
  let (result:KeyValuePair<bigint, bigint list>) = limit |> hailstoneSequences |> longestSequence
  printf "Number: %O, Sequence Length: %O\n" result.Key result.Value.Length
  printf "Sequence: %A\n" result.Value

memoizedSolution 1000000I

// ----------------------------------------------------
// Async Version:
let longestHailstoneSequenceAsync limit =
  let sequences = Dictionary<int, bigint list>()
  let _addSequence n (dictionary:Dictionary<int, bigint list>) =
    dictionary.Add(n, (hailstoneSequence n))
  let tasks = [1..limit] |> List.map (fun n -> async { _addSequence n sequences })
  printf "Composed %d tasks\n" tasks.Length
  tasks |> Async.Parallel |> Async.RunSynchronously |> ignore
  printf "******* Done computing sequences\n"
  printf "******* Finding the longest sequence\n"
  Seq.maxBy (fun (entry:KeyValuePair<int, bigint list>) -> entry.Value.Length) sequences

let asyncSolution limit =
  let (result:KeyValuePair<int, bigint list>) = longestHailstoneSequenceAsync limit
  printf "Number: %d, Sequence Length: %d\n" result.Key result.Value.Length
  printf "Sequence: %A\n" result.Value

// Async version was significantly slower than the sync one. Manages to use multiple (2-3, not all 8) cores, but still very very slow.
// -----------------------------------------------------
