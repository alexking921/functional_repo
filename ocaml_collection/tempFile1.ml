

(*functions 
OCT 22 


Implementing a stack (simulating OOP through Ocaml)
*)

(*base is the type of objects in our stack, like a stack of ints or a stack of strings etc
--we use 'base definition so we can create any type of stack as opposed to being limited to only stack of 
say ints and having to create a whole new type if we wanted a stack of strings





PRE-IMPLEMENTATION type definitions
 *)




exception StackError;;
type 'base stackOperation = 
    IsEmpty |
    Peek |
    Pop |
    Push of 'base ;;


type 'base stackResult = 
    BoolResult of bool | (*for IsEmpty return *)
    BaseResult of 'base | (*for Peek return *)
    NoResult ;;  (*for operations that return nothing like pop-- pop only changes the stack but doesn't return the element it 'popped' *)


(*creating our own list type here. we could of just used Ocamls but didn't *)
type 'base stackList =
    EmptyStack |
    NonEmptyStack of 'base * 'base stackList ;;


(* THIS GENERAL DEFINITION OF STACK IS NOT USED IN HIS ORIGINAL CODE
type 'base stack =
    'base stackOperation -> 'base stackResult;;











    START OF IMPLEMENTATION below


    := is OCamls version of the simple assignment operator like in Java: 'int myNum = 5' 
*)


let makeStack () =
    let top = ref EmptyStack in 
    let isEmpty () =
        BoolResult (!top = EmptyStack) 
    in let peek () = 
        match !top with
        EmptyStack -> raise StackError |
        NonEmptyStack (first, _) -> BaseResult first
        in let pop () = 
            match !top with
            EmptyStack -> raise StackError |
            NonEmptyStack (_, rest) -> top := rest ; NoResult
            in let push base = 
                top := NonEmptyStack (base, !top) ; NoResult
                in let dispatch operation =
                    match operation with
                    IsEmpty -> isEmpty () |   (*notice the distinction in capitalization from lHS -> RHS *)
                    Peek -> peek () |
                    Pop -> pop () |
                    Push base -> push base
                in dispatch;;

let s = makeStack () ;;
(* RETURN TYPE defined below *)
(*
val s: '_a stackOperation -> '_a stackResult = <fun>
*)

(*
EXAMPLE CALL:
# s (Push "a");; (this wont return anything obvi)
# s ;;
-: string stackOperation -> string stackResult = <fun>
*)

(*)
OCaml will use the first type that is added to the stack as a way to define what type the 
stack should be, so all further elements added must be of that same type as the first one 
*)


























(* 
--
--
--
_______NOV 5TH NOTES______________________________________________________ 
--
--
--
*)



(* SIMULATING LAZY EVALUATION  *)

(*EAGER *)
let mul x y =
    if x = 0
    then 0
    else if y = 0
        then 0
        else x*y;;
    
    (*
    use (fun () -> e)
    *)

(*LAZY 1 *)
let anonymous_mul x y =
    if x() = 0
    then 0
    else if y() = 0
        then 0 
        else x() * y();;


(*example call for anonymous-mull*)
anonymous_mul (fun() -> 3) (fun() -> 72);;

(* *)

(* LAZY 2  (less calls to the anon functions) *)
let anonymous_mul x y =
    let x' = x() in if x' = 0
                    then 0
                    else let y' = y()
                        in if y' =0
                        then 0
                        else x' *y';;







(* Data structures for lazy evalution*)


let demand future = 
    match !future with
    Value (result) -> result |
    Function (func) -> let temp = func () 
                        in future := Value (temp) ; temp;;

let delay funct =
    ref Function (func);;

    delay (fun() -> fib 46)

type 'base delayed =
    Value of 'base |
    Function of (() -> 'base);;


type 'base future = 'base delayed ref;;  

(*the result of all this is you get memoization for free *)



(*LAZY 3 (using the demand() function stuff) *)
let future_mul x y = 
    if demand x = 0
    then 0
    else if demand y = 0
        then 0
        else demand x * demand y ;; 







