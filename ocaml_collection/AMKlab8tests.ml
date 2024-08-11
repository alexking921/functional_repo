(*
  Tests for CSci 2041 Lab 8

  45 points.

  Alex King (no partner)
*)

(* HASH. Return the index of a bucket in TABLE where KEY may be found. TABLE is
   made by calling HASH MAKE. *)

type ('key, 'value) pair = 
  NoPair |
  Pair of 'key * 'value ref * ('key, 'value) pair ref;;

let hash table key =
  abs ((Hashtbl.hash key) mod (Array.length table)) ;;

(* HASH MAKE. Return an array to be used as a hash table. It contains MODULUS
   buckets. For various obscure reasons involving number theory, the hash table
   works best if MODULUS is a "large" prime number. *)

let hashMake modulus =
  Array.make modulus NoPair ;;


exception NoSuchKey;;


let hashHas table key =
  let bucketIndex = table.(hash table key) in 
    let rec searching bucket =
      match bucket with 
      NoPair -> false |
      Pair(a, b, c) -> if a = key 
                        then true
                        else searching (! c)
  in searching bucketIndex ;;



let hashGet table key =
  let bucketIndex = table.(hash table key) in
    let rec searching bucket =
      match bucket with 
      NoPair -> raise NoSuchKey |
      Pair(a, b, c) -> if a = key 
                        then !b
                        else searching (! c)
  in searching bucketIndex ;;


let hashPut table key value = 
  let bucketIndex = table.(hash table key) in
    let rec searching bucket = 
      match bucket with
      NoPair -> (table.(hash table key) <- Pair(key, ref value, ref bucketIndex)) |
      Pair(a, b, c) -> if a = key
                      then b := value
                      else searching (!c)
  in searching bucketIndex;;




let hashDelete table key =
  let bucketIndex = table.(hash table key) in
    let rec deleting d =
      match d with 
      NoPair -> NoPair | 
      Pair (a,b,c) -> if a = key then (!c ) 
                      else ((c := deleting (!c)) ; d)
  in table.(hash table key) <- deleting bucketIndex;;




(* Tests, worth 45 points. Note that the table size is small enough that you
   can see the entire table by typing "table ;;" to the OCaml REPL. *)

let table = hashMake 23 ;;                        (* [| NoPair ... |] 0 pt. *)

hashHas table "uno" ;;                            (* false            3 pt. *)

hashPut table "uno" "one" ;;                      (* ()               3 pt. *)

hashHas table "uno" ;;                            (* true             3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)

hashPut table "duo" "two" ;;                      (* ()               3 pt. *)

hashPut table "trio" "three" ;;                   (* ()               3 pt. *)

hashHas table "trio" ;;                           (* true             3 pt. *)

hashDelete table "duo" ;;                         (* ()               3 pt. *)

hashHas table "duo" ;;                            (* false            3 pt. *)

hashPut table "duo" "bleen" ;;                    (* ()               3 pt. *)

hashGet table "duo" ;;                            (* "bleen"          3 pt. *)

hashGet table "trio" ;;                           (* "three"          3 pt. *)

hashHas table "bleen" ;;                          (* false            3 pt. *)

hashDelete table "bleen" ;;                       (* ()               3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)


(*

MY TEST RESULTS: 

# #use "AMKlab8tests.ml";;
type ('key, 'value) pair =
    NoPair
  | Pair of 'key * 'value ref * ('key, 'value) pair ref
val hash : 'a array -> 'b -> int = <fun>
val hashMake : int -> ('a, 'b) pair array = <fun>
exception NoSuchKey
val hashHas : ('a, 'b) pair array -> 'a -> bool = <fun>
val hashGet : ('a, 'b) pair array -> 'a -> 'b = <fun>
val hashPut : ('a, 'b) pair array -> 'a -> 'b -> unit = <fun>
val hashDelete : ('a, 'b) pair array -> 'a -> unit = <fun>
val table : ('_weak1, '_weak2) pair array =
  [|NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair;
    NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair; NoPair;
    NoPair; NoPair; NoPair; NoPair; NoPair|]
- : bool = false
- : unit = ()
- : bool = true
- : string = "one"
- : unit = ()
- : unit = ()
- : bool = true
- : unit = ()
- : bool = false
- : unit = ()
- : string = "bleen"
- : string = "three"
- : bool = false
- : unit = ()
- : string = "one"
# 






 *)

