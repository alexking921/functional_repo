

the really slow default fib function
let rec fib0 n =
    match n with
        0 -> 0 |
        1 -> 1 |
        _ -> fib (n-2) + fib (n-1);;



fib function that puts 'already calculated' values in an array, which drastically cuts down on the duplicate computatations that make up most of the slow-fibs operations
(HIGHER ORDER FUNCTION)
array initially filled with all negative 1's, as you can see on line2, then we use the fact that 'unaltered' elements are 'negative' so test if we should exe or move onto next elem
takes an array of length L, then returns a fib function that uses that array when computing 
let makeFib l = 
    let t = Array.make l (-1)
        in let rec fibbing n =
            if t.(n) >= 0
            then t.(n)
            else (t.(n) <- 
                            (match n with
                            0 -> 0|
                            1 -> 1 |
                            _ -> fibbing (n-2) + fibbing (n-1));
                        e.(n))
            in fibbing ;;            (*notice we're not passing any value to fibbing here, bc we want to return the FUNCTION fibbing, not return the result of fibbing and some arg passed to it *)

let fib1 = makeFib 50 ;; this returns a FUNCTION

time "testing fib1" fib1 46;;    
(this takes only 0.000004 seconds to compute result)        
        

// returns the time it takes to compute a fib number of 'x', (not sure if it's the time relative to the default non-memoization version of fib)
let time s f x =
    let t0 = Sys.time ()
    in let y = f x
        in let t1 = Sys.time ()
            in Printf.printf "%s %f seconds\n" s (t1 - t0) ; 
                y;;

example call:
time "testing fib0" fib0 46 ;;             (this will print the time it takes to compute fib 46)
 (this one took 86 seconds to output the result)






OCAML ARRAYS
absurd syntax, as expected

[| 1;3;4;5;7 |]

GENERAL ARR OPERATIONS

Array.make 5 2;;                this call makes an array with 5 elements, each element being a 2    ==>>  [| 2; 2; 2; 2; 2 |]

Array.length a

a.(j)                           returns element in Array at index j

a.(j) <- e                      changes the element in array at index j, to 'e'         Note it's NOT:  a.(j) = e

                        





type pair =













