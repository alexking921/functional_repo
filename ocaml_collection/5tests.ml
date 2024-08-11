(*
  CSci 2041 Lab Assignment 5

Alex King (No partner) 

    James Moen
    12 Oct 21

  It's worth 30 points.
*)

(* PROPOSITION. An expression in propositional logic using '¬', '∧', and '∨'.

   false        ↝  False
   true         ↝  True
   a, b, c ...  ↝  Var "a", Var "b", Var "c" ...
   α ∧ β        ↝  And (α, β)
   α ∨ β        ↝  Or (α, β)
   ¬ α          ↝  Not α

   The squiggly arrow '↝' means "represented as." *)

type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition ;;



   let rec unorify p = 
    match p with
    Var x -> Var x |

    Not (Or (x, y)) -> unorify (And (Not x, Not y))|

    Or (Not (Not (x)), y) -> unorify (Or (x, y)) |
    Or (x, Not (Not (y))) -> unorify (Or (x, y)) |
    Or (x, y) -> unorify (Not (And (Not (x), Not (y))))|

    And (Not (Not (x)), y) -> unorify (And (x, y)) |
    And (x, Not (Not (y))) -> unorify (And (x, y)) |
    Not (Not (x)) -> unorify (x)|

    Not (And (Not (Not x), y)) -> unorify (Not (And (x,y))) |
    Not (And (x, Not (Not y))) -> unorify (Not (And (x,y)))|
    _ -> p;;








(*

I created functions for each testing case so it was easier to troubleshoot
TEST VALUES: 
*)

let test1 = Var "a";;      
let test2 = Not (Var "a");;     
let test3 = Not (Not (Var "a"));;       
let test4 = Not (Or (Var "x", Var "y"));;       
let test5 = Not (And (Var "a", Var "b"));;      
let test6 = Or (Not (Var "a"), Not (Var "b"));;     
let test7 = Or (Not (Not (Var "a")), Not (Var "b"));;




(* Unorify the proposition a. *)

unorify test1;;

(* 2 points if you get: Var "a" *)






(* Unorify the proposition ¬ a. *)

unorify test2;;

(* 3 points if you get: Not (Var "a") *)






(* Unorify the proposition ¬ ¬ a. *)

unorify test3;;

(* 5 points if you get: Var "a" *)






(* Unorify the proposition ¬ (a ∨ b). *)

unorify test4;;

(* 5 points if you get: And (Not (Var "a"), Not (Var "b")) *)






(* Unorify the proposition ¬ (a ∧ b). *)

unorify test5;;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ a ∨ ¬ b. *)

unorify test6;;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)

unorify test7;;

(* 5 points if you get: Not (And (Not (Var "a"), Var "b")) *)













(*
utop # unorify test1;;
- : proposition = Var "a"
─( 15:21:13 )─< command 30 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test2;;
- : proposition = Not (Var "a")
─( 15:24:40 )─< command 31 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test3;;
- : proposition = Var "a"
─( 15:24:47 )─< command 32 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test4;;
- : proposition = And (Not (Var "x"), Not (Var "y"))
─( 15:24:55 )─< command 33 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test5;;
- : proposition = Not (And (Var "a", Var "b"))
─( 15:25:00 )─< command 34 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test6;;
- : proposition = Not (And (Var "a", Var "b"))
─( 15:25:25 )─< command 35 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # unorify test7;;
- : proposition = Not (And (Not (Var "a"), Var "b"))
─( 15:25:53 )─< command 36 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # 
*)
