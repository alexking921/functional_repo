(*
Alex King (no partner) Lab 10

  CSci 2041 Lab Assignment 10 Tests

    James Moen
    21 Nov 21

  Tests are worth 34 points.
*)

(* THING. The type of a Pure Lisp object, from the lectures. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

open Printf;;

exception BadThing;;

let rec printingThing thing =
  let rec printingThings things =
    match things with
    | Nil -> ()
    | Cons(a,b) -> printf " " ; printingThing a ; printingThings b
    | _ -> raise BadThing
  in 
      match thing with
      | Closure(a,b,c) -> printf "[Closure]"
      | Cons(a,b) -> printf "(" ; printingThing a ; printingThings b ; printf ")"
      | Nil -> printf "nil"
      | Number n -> printf "%i" n
      | Primitive y -> printf "[Primitive]"
      | Symbol str -> printf "%s" str;;
    

let printThing thing =  printingThing thing;
    printf "\n";;

(* Tests begin here, simple ones first. The comments say what must be printed
   to get the points. Note that case matters, all the blanks must be in their
   right places, there must be no extra blanks, and everything printed must
   end with a newline. We don't care what PRINT THING returns, only what it
   prints. *)

printThing Nil ;;                                             (* 2 pts. nil *)

printThing (Number 7734) ;;                                  (* 2 pts. 7734 *)

printThing (Symbol "lobyms") ;;                            (* 2 pts. lobyms *)

printThing (Closure (Nil, Nil, [])) ;;                  (* 2 pts. [Closure] *)

printThing (Primitive (fun _ _ -> Nil)) ;;            (* 2 pts. [Primitive] *)

(* More complex tests involving lists. *)

printThing                                                    (* 2 pts. (a) *)
  (Cons (Symbol "a", Nil)) ;;

printThing                                                  (* 2 pts. (a b) *)
  (Cons (Symbol "a", Cons (Symbol "b", Nil))) ;;

printThing                                                (* 2 pts. (a b c) *)
  (Cons (Symbol "a",
     Cons (Symbol "b",
       Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a) b c) *)
  (Cons (
     Cons (Symbol "a", Nil),
     Cons (Symbol "b",
       Cons (Symbol "c", Nil)))) ;;

printThing                                              (* 2 pts. ((a b) c) *)
  (Cons (
     Cons (Symbol "a",
       Cons (Symbol "b", Nil)),
     Cons (Symbol "c", Nil))) ;;

(* 2 pts. (a (b c)) *)
printThing                                              
  (Cons (Symbol "a",
    Cons (
      Cons(Symbol "b", Cons (Symbol "c", Nil)),
      Nil))) ;;

(* 2 pts. ((a b c)) *)
printThing                                              
  (Cons (
    Cons (Symbol "a",
      Cons (Symbol "b",
        Cons (Symbol "c", Nil))),
    Nil)) ;;

(* The big finish. All that horror below is the internal OCaml form of Pure
   Lisp code that defines the factorial function. It looks like this when it's
   properly indented.

   (define !
     (lambda (n)
       (if
         (= n 0)
         1
         (∗ n (! (- n 1))))))

   Your function PRINT THING need not print in indented form, so it will print
   this instead, for 10 pts.

   (define ! (lambda (n) (if (= n 0) 1 (∗ n (! (- n 1))))))
*)




printThing
  (Cons (Symbol "define",
    Cons (Symbol "!",
     Cons
      (Cons (Symbol "lambda",
        Cons (Cons (Symbol "n", Nil),
         Cons
          (Cons (Symbol "if",
            Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
             Cons (Number 1,
              Cons
               (Cons (Symbol "*",
                 Cons (Symbol "n",
                  Cons
                   (Cons (Symbol "!",
                     Cons
                      (Cons (Symbol "-",
                        Cons (Symbol "n", Cons (Number 1, Nil))),
                      Nil)),
                   Nil))),
               Nil)))),
          Nil))),
      Nil)))) ;;



(* Soon we will write a function that reads Lisp expressions, so we don't have
   to look at things like this any more. 
*) 


(* MY TEST RESULTS from terminal:
    # #use "lab10.ml";;
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
exception BadThing
val printingThing : thing -> unit = <fun>
val printThing : thing -> unit = <fun>
nil
- : unit = ()
7734
- : unit = ()
lobyms
- : unit = ()
[Closure]
- : unit = ()
[Primitive]
- : unit = ()
(a)
- : unit = ()
(a b)
- : unit = ()
(a b c)
- : unit = ()
((a) b c)
- : unit = ()
((a b) c)
- : unit = ()
(a (b c))
- : unit = ()
((a b c))
- : unit = ()
(define ! (lambda (n) (if (= n 0) 1 (* n (! (- n 1))))))
- : unit = ()
*) *)