(*Cons operator *)
let list = 3::[1; 7500; 62; 90];; (*this returns a LIST with 3 as the new first element [3; 1; 7500; 62; 90] *)

append [1;2;3] [4;5]



let rec appending left right =
    if left = []
    then right
    else (hd left) :: appending(tl left) right ;;
(*OUTPUT TYPE:  ??????    *)

let append left right =
    if right = []
    then left
    else appending left right ;;

append [1;2;3] [4;5];; (*THE CALL *)
appending [1;2;3] [4;5];;
1:: appending [2;3] [4;5];;
1::(2:: appending [3] [4;5])
1::(2::(3:: appending [] [4;5]))
1::(2::(3:: [4;5])) -->> (*this line evaluates to the final answer [1;2;3;4;5] *)
(*notice how the right side of :: may look complex but it always evaluates to a LIST type as required *)









(*Examples of good/diff ways to use Match-With *)

(*accepts a char and returns true if said char is a vowel, false otherwise *)
(*note how we used all those 'or' operators for just 1 rule and there's only a total of 2 rules
even though it looks like there could be 10 if you were judging by the number of | | | | *)
(*also quick note how we used wild card in rule 2 to catch-all other inputs *)
let isVowel c =
    match c with
    'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u' | -> true |
    _ -> false;;



(*Note how we define the pattern to match the 2-length tuple that the function is dealing with  *)
let fst t = 
    match t with
    (x,y) -> x;;

let snd t = 
    match t with
    (x,y) -> y;;



(*this function accepts a digit and returns the string of that digit. So passing 1 would return the string "one" and passing 2 returns "two" etc
Also notice again the wild-card used for catch-all invalid input scenarios *)
let digitName d =
    match d with
    0 -> "zero" |
    1 -> "one" |
    2 -> "two" |
    _ -> "unknown" ;;






(*printing a list *)
let t = [1;2;3];;
List.map print_int t;;

(*built-in hd and tl functions for lists *)
(* hd() accepts a LIST and returns the first ELEMENT in that list *)
(* tl() accepts a LIST and returns a LIST with first ELEMENT removed*)
let myList =[1;2;3];;
hd myList;; -->> returns 1
tl myList;; -->> returns [2;3]







let epsilon = 0.00001;;

let squirt a =
    let rec squirting g h =
    if abs_float(g -.h) >= epsilon
    then let g' = (g +. h)/. 2.0
        in let h' = a/g'
            in squirting g' h'
    else g
in squirting 1.0 a ;;




































(**Use pattern-matching to define a recursive function reverse, which takes in
a list as its only argument and returns a list in reverse order. For example:
calling it on the list [1;2;3] should return [3;2;1]. *)
open List;;





let rec reversing reversed unreversed =
    if unreversed = []
    then reversed
    else reversing ((hd unreversed)::reversed) (tl unreversed);;
(*OUTPUT TYPE:  val reversing : 'a list -> 'a list -> 'a list = <fun>    *)

let reverse objects = 
    reversing [] objects
    ;;
(*OUTPUT TYPE: val reverse : 'a list -> 'a list = <fun> *)



let reverse myList =
    let rec reversing unrevList revList  =
        match unrevList with
            [] -> revList |
            head::tail -> reversing (head::revList) tail 
    in reversing myList [];;
    





(3) +. (5);;    (* error *)
(3) +. (2.3);;  (* error *)
(3.3) +. (2);;  (* error *)
(3.3) +. (2.3);;(* 5.6 *)

(3) + (5);;     (* 8 *)
(3) + (2.3);;   (* error *)
(3.3) + (2);;   (* error *)
(3.3) + (2.3);; (* error *)




(*)
let reverse myList =
    let rec reversing unrevList revList  =
        match unrevList with
            [] -> revList |
            head::tail -> hd::revList
        reversing tl(unrevList) revList
    in reversing myList [];;
(** *)
*)

(*)
let rec factorial n =
    if n == 1
    then 1
    else n * factorial n-1;; 

let rec factorial n =
    factorial 1
(*)*))*)


let rec factorialNonTail n = 
    if n <= 0
    then 0
    else n*factorialNonTail n-1;;

let rec factorialTail n  =
    let facing tot = 1
    in facing n
    if n <= 0
    then t
    else factorialTail (n-1) (n*t);;

(*THIS ONE WORKS *)
let reverse l =
    let rec reversing list m =
        match list with
        [] -> m |
        h::t -> reversing t (h::m)
    in reversing l [];;



(*THIS ONE WORKS *)
let reverse l =
    let rec reversing unreversed reversed =
        match unreversed with
        [] -> reversed |
        h::t -> reversing t (h::reversed)
    in reversing l [];;



    (*THIS ONE WORKS *)
let reverse l =
    let rec reversing reversed unreversed =
        match unreversed with
        [] -> reversed |
        h::t -> reversing (h::reversed) t
    in reversing [] l;;


(*flipping the order of the args for the helper function: *)


(*change 'rev' to 'reversing' *)
(*change 'l to objects' to 'reversing' *)
(*change 'm' to 'unreversed' *)


(*'list' is the UNreversed list' *)
(*'m' is the reversed list' *)







let fst t = 
    match t with
    (x,y) -> x;;

let snd t = 
    match t with
    (x,y) -> y;;





(*4.	
Define the function removeValueFromKey, either imperatively or functionally, which takes in a 
list of key-value pairs (stored as tuples where the first part is the key and the second is a list
 including all paired values), a key, and a value and returns a new list of key-value pairs without 
 the targetValue in the valueList for the given targetKey (assuming both the indicated key and value 
 are present).  
 
 ( , [] )
 (key1, [list; of; all; values; paired; w; this; key] )

 *)
(** *)
(key1, [valueList] )
(** *)

(*this testList has 3 elements, each of those 3 elements is a 2-sized TUPLE, 
...these TUPLES are defined as having an int on leftside, and a list of ints on rightside *)
let testList = [(4, [1000,2000]); (2, [5000,8000]); (7, [6000,9000])];;
 
 (** here's a sample call: *)
 (*so we want to remove the  *)
let removeValueFromKey listOfKeyValPairs targetKey targetValue =
    let rec removing list =
        if fst(hd(list)) == targetKey (*this means you found the right tuple *)
        then [(4,[9999])]
        else removing tl(list)
    in removing listOfKeyValPairs;;


(** here's a sample call: *)  
removeValueFromKey testList 4 1000;;     (*this means we want to remove '2000' from the list within the first TUPLE  *)

(** FIRST iterate through the list to find the tuple with key value of '4' (maybe wanna use fst, snd) *)









(** Assoiciation list stuff *)

(*making an associate list  *)(* notice how you're defining not just the tuple type, but also the list itself which will hold all the tuples *)
type ('key, 'value) al = ('key * 'value) list;; 

exception NoSuchKey;;

let alGet key pairs =
    let rec alGetting pairs =
        match pairs with
        [] -> raise NoSuchKey |
        (otherKey, otherValue):: otherPairs -> 
                                            if key = otherKey 
                                            then otherValue
                                            else alGetting otherPairs
    in alGetting pairs;;




let alPut pairs key value =
    (key, value):: pairs;;




(*demonstration of the functions/ data structure:  *)

let words = [("alex", 7); ("king", 6); ("vikes", 20); ("packers", 14); ("vegeta", 3)];; 

alGet "alex" words;;  (*returns 7 *)

alPut words "lebron" 23;; (*returns list 'words' except with this new lebron,23 pair as the new first element *)























