(*
  CSci 2041 Lab 6 Tests

Alex King (Parter with Abigail Bliss)

    James Moen
    17 Oct 21

  These tests are worth 30 points.
*)

(* MAKE STREAM. Return a new stream. THIS is the first element of the stream.
   STATE is an object that stores the stream's state somehow. The function NEXT
   takes THIS and STATE as arguments. It returns a 2-tuple with a new THIS and
   a new STATE inside it. *)

let makeStream this state next =
  ((this, state), next) ;;

(* FIRST. Return the first element of a stream. *)

let first ((this, state), next) =
  this ;;

(* REST. Return a stream with its first element removed. *)

let rest ((this, state), next) =
  (next this state, next) ;;

(* TAKE. Return a list of the first COUNT elements TAKEn from STREAM. *)

let rec take count stream =
  match count
  with 0 -> [] |
       _ -> (first stream) :: take (count - 1) (rest stream) ;;

(* NATURALS. A infinite stream of 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ... We don't use
   STATE here, so we let it be the dummy unit object (). *)

let naturals =
  makeStream 0 () (fun this state -> (this + 1, ())) ;;







let odds = 
    makeStream 1 () (fun this state -> (this + 2, ()));;


let rec trim count stream =
    match count with
    0 -> stream |
    _ -> trim (count-1) (rest stream);;



let scale factor stream =
    makeStream ((first stream)*factor) (rest stream) (fun this state -> ((first state)*factor), rest state) ;;


let sum left right =
    makeStream ((first left) + (first right)) (rest left, rest right)
                    (fun this state -> (match state with 
                        (stateLeft, stateRight) -> (((first stateLeft) + (first stateRight)), (rest stateLeft, rest stateRight))));;






first odds ;;                     (* 1  2 pt. *)

first (rest odds) ;;              (* 3  2 pt. *)

first (rest (rest odds)) ;;       (* 5  2 pt. *)

take 7 odds ;;                    (* [1; 3; 5; 7; 9; 11; 13]  2 pt. *)

let but1st5 = trim 5 naturals ;;

first but1st5 ;;                  (* 5  2 pt. *)

first (rest but1st5) ;;           (* 6  2 pt. *)

first (rest (rest but1st5)) ;;    (* 7  2 pt. *)

take 7 but1st5 ;;                 (* [5; 6; 7; 8; 9; 10; 11]  2 pt. *)

let byFives = scale 5 naturals ;;

first byFives ;;                  (* 0   2 pt. *)

first (rest byFives) ;;           (* 5   2 pt. *)

first (rest (rest byFives)) ;;    (* 10  2 pt. *)

take 7 byFives ;;                 (* [0; 5; 10; 15; 20; 25; 30]  2 pt. *)

let natsPlusByFives = sum naturals byFives ;;

first natsPlusByFives ;;          (* 0   2 pt. *)

first (rest natsPlusByFives) ;;   (* 6   2 pt. *)

take 7 natsPlusByFives ;;         (* [0; 6; 12; 18; 24; 30; 36]  2 pt. *)











(*

MY TEST RESULTS:

# #use "lab6testrun.ml";;
val makeStream : 'a -> 'b -> 'c -> ('a * 'b) * 'c = <fun>
val first : ('a * 'b) * 'c -> 'a = <fun>
val rest : ('a * 'b) * ('a -> 'b -> 'c) -> 'c * ('a -> 'b -> 'c) = <fun>
val take : int -> ('a * 'b) * ('a -> 'b -> 'a * 'b) -> 'a list = <fun>
val naturals : (int * unit) * (int -> '_weak1 -> int * unit) =
  ((0, ()), <fun>)
val odds : (int * unit) * (int -> '_weak2 -> int * unit) = ((1, ()), <fun>)
val trim :
  int ->
  ('a * 'b) * ('a -> 'b -> 'a * 'b) -> ('a * 'b) * ('a -> 'b -> 'a * 'b) =
  <fun>
val scale :
  int ->
  (int * 'a) * (int -> 'a -> 'b) ->
  (int * ('b * (int -> 'a -> 'b))) *
  ('c -> (int * 'd) * (int -> 'd -> 'e) -> int * ('e * (int -> 'd -> 'e))) =
  <fun>
val sum :
  (int * 'a) * (int -> 'a -> 'b) ->
  (int * 'c) * (int -> 'c -> 'd) ->
  (int * (('b * (int -> 'a -> 'b)) * ('d * (int -> 'c -> 'd)))) *
  ('e ->
   ((int * 'f) * (int -> 'f -> 'g)) * ((int * 'h) * (int -> 'h -> 'i)) ->
   int * (('g * (int -> 'f -> 'g)) * ('i * (int -> 'h -> 'i)))) =
  <fun>
- : int = 1
- : int = 3
- : int = 5
- : int list = [1; 3; 5; 7; 9; 11; 13]
val but1st5 : (int * unit) * (int -> unit -> int * unit) = ((5, ()), <fun>)
- : int = 5
- : int = 6
- : int = 7
- : int list = [5; 6; 7; 8; 9; 10; 11]
val byFives :
  (int * ((int * unit) * (int -> unit -> int * unit))) *
  ('_weak3 ->
   (int * '_weak4) * (int -> '_weak4 -> '_weak5) ->
   int * ('_weak5 * (int -> '_weak4 -> '_weak5))) =
  ((0, ((1, ()), <fun>)), <fun>)
- : int = 0
- : int = 5
- : int = 10
- : int list = [0; 5; 10; 15; 20; 25; 30]
val natsPlusByFives :
  (int *
   (((int * unit) * (int -> unit -> int * unit)) *
    ((int * ((int * unit) * (int -> unit -> int * unit))) *
     (int ->
      (int * unit) * (int -> unit -> int * unit) ->
      int * ((int * unit) * (int -> unit -> int * unit)))))) *
  ('_weak6 ->
   ((int * '_weak7) * (int -> '_weak7 -> '_weak8)) *
   ((int * '_weak9) * (int -> '_weak9 -> '_weak10)) ->
   int *
   (('_weak8 * (int -> '_weak7 -> '_weak8)) *
    ('_weak10 * (int -> '_weak9 -> '_weak10)))) =
  ((0, (((1, ()), <fun>), ((5, ((2, ()), <fun>)), <fun>))), <fun>)
- : int = 0
- : int = 6
- : int list = [0; 6; 12; 18; 24; 30; 36]
# 






*)
