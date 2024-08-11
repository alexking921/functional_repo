(*
    CSCI 2041 Moen
    Tautology Project
    Alex King
    Student ID# 3863095 

    I included test cases for all the scenarios And(), Or(), Imply(), Equiv() as well
    as False, True, Not() 

*)

type proposition =
    False |
    True |
    Var of string |
    And of proposition * proposition |
    Or of proposition * proposition |
    Not of proposition |
    Imply of proposition * proposition |
    Equiv of proposition * proposition |
    If of proposition * proposition * proposition ;;

(*THE FUNCTIONS *)

let rec substitute c v b = 
    match c with
    If (x, y, z) -> If ((substitute x v b), (substitute y v b), (substitute z v b)) |
    a -> if a = v 
        then b
        else c
    ;;

let rec simplify c =
    match c with
    If (x, a, b) -> let simplified_a = simplify (substitute a x True)
                    in let simplified_b = simplify (substitute b x False) 
                        in if simplified_a = True && simplified_b = False then simplify x
                        else if x = True then simplify simplified_a
                        else if x = False then simplify simplified_b
                        else if simplified_a = simplified_b then simplify a
                        else If (x, simplified_a, simplified_b) |
    _ -> c;;

let rec normalize c =
    match c with
        If ((If (v, w, x)), y, z) -> normalize ((If (normalize (v), normalize ((If (normalize (w), normalize (y), normalize (z)))), normalize ((If (normalize (x), normalize (y), normalize (z))))))) |
        _ -> c;;

let rec ifify p =
    match p with
        Not (a) -> (ifify (If (ifify (a), False, True)))|
        And (a, b) -> (ifify (If (ifify (a), ifify (b), False)))|
        Or (a, b) -> (ifify (If (ifify (a), True, ifify (b)))) |
        Imply (a, b) -> (ifify (If (ifify (a), ifify (b), True)))|   
        Equiv (a, b) -> (ifify (If (ifify (a), ifify (b), (If (ifify (b), False, True)))))|
        _ -> p
        ;;

let tautology p = 
    let result = simplify (normalize (ifify p))
    in if result = True then true
    else false;;







(* TEST CASES AND RESULTS: *)
(* The first 5 cases ARE tautologies: 
(There's a test for Not(), And(), Or(), Imply(), Equiv() as well as True, False and Var *)

(*                                IMPLY-test *)
let test1 = Imply (Not (And (Var "a", Var "b" )), Or (Not (Var "a"), Not (Var "b")));;

(*         (A v Not-A)          OR-test *)
let test2 = Or (Var "a", Not (Var "a"));;

(*            (A v Not-A) ^ (B v Not-B)                     AND-test *)
let test3 = And ((Or (Var "a", Not (Var "a"))), (Or (Var "b", Not (Var "b"))))

(*    ( A v Not-A ) <-> ( B v Not-B )          EQUIV-test   *)
let test4 = Equiv ((Or (Var "a", Not (Var "a")), (Or (Var "b", Not (Var "b")))));;

(* True                  True-test *)
let test5 = True;;


(* The remaining cases are NOT tautologies: *)

(*          (A ^ B)       AND-test   *)
let test6 = And (Var "a", Var "b");;


(*          (A v B) v (B v A)     not a taut because F F case    OR-test   *)
let test7 = Or (Or (Var "a", Var "b"), (Or (Var "b", Var "a")));;


(*   Not-(A ^ B)->(A v B)               IMPLY-test *)
let test8 = Imply (Not (And (Var "a", Var "b" )), Or ( Var "a", Not (Var "b")));;


(*              ( A ^ Not-B ) <-> ( B v Not-B )      EQUIV-test *)
let test9 = Equiv ((And (Var "a", Not (Var "a")), (Or (Var "b", Not (Var "b")))));;


(*   Not-(A v Not-A)        Not-A    NOT-test, also a contradiction test (always-false) *)
let test10 = Not (Or (Var "a", Not (Var "a")));;

(* False            True-test *)
let test11 = False;;

(*Var-test *)
let test12 = Var "a";;


tautology test1;;
tautology test2;;
tautology test3;;
tautology test4;;
tautology test5;;
tautology test6;;
tautology test7;;
tautology test8;;
tautology test9;;
tautology test10;;
tautology test11;;
tautology test12;;







(* RESULTS  

val test1 : proposition =
  Imply (Not (And (Var "a", Var "b")), Or (Not (Var "a"), Not (Var "b")))
val test2 : proposition = Or (Var "a", Not (Var "a"))
val test3 : proposition =
  And (Or (Var "a", Not (Var "a")), Or (Var "b", Not (Var "b")))
val test4 : proposition =
  Equiv (Or (Var "a", Not (Var "a")), Or (Var "b", Not (Var "b")))
val test5 : proposition = True
val test6 : proposition = And (Var "a", Var "b")
val test7 : proposition = Or (Or (Var "a", Var "b"), Or (Var "b", Var "a"))
val test8 : proposition =
  Imply (Not (And (Var "a", Var "b")), Or (Var "a", Not (Var "b")))
val test9 : proposition =
  Equiv (And (Var "a", Not (Var "a")), Or (Var "b", Not (Var "b")))
val test10 : proposition = Not (Or (Var "a", Not (Var "a")))
val test11 : proposition = False
val test12 : proposition = Var "a"
- : bool = true
- : bool = true
- : bool = true
- : bool = true
- : bool = true
- : bool = false
- : bool = false
- : bool = false
- : bool = false
- : bool = false
- : bool = false
- : bool = false
# 






*)










