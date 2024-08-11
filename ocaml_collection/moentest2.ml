(*
  DELETER.ML. Show how to delete nodes from a singly linked mutable list.

    James Moen
    31 Oct 21

  This was an example shown in lecture last week, with slight changes.
*)



let ptr = ref 3 ;;
let defer = !ptr ;;
(* result from running it in ocaml:
val ptr : int ref = {contents = 3}
val defer : int = 3
*)


let rec fac n =
  if n > 1 
  then n * fac(n-1)
  else 1;;

let fac n = 
  let rec facing f n =
  if n<=0 
  then f
  else facing (n*f) (n-1)
  in facing 1 n;;



(* MUTY LIST. A singly linked linear list of nodes that contain BASEs, linked
   together by mutable pointers. Each MUTY LIST has a head node. *)

type 'base mutyList =
  EmptyMutyList |
  NonemptyMutyList of 'base * 'base mutyList ref ;;



(* MUTY LIST ERROR. Raised when something bad happens. The STRING tells us what
   the bad thing was. *)

exception MutyListError of string ;;



(*TRYING TO WRITE FUNCTION THAT ADDS NODE TO END OF the SINGLY-LINKED LIST *)
let addNodeToEndOfList headNode myNode =
  let rec traverse headNode =
    match headNode with
      EmptyMutyList -> raise (MutyListError "Missing head node.")  |
      NonemptyMutyList (a, b) -> if !b = EmptyMutyList  
                                  then b := myNode     
                                  else traverse !b
  in traverse headNode;;
                      


(*TRYING TO WRITE FUNCTION THAT ADDS NODE TO FRONT OF the SINGLY-LINKED LIST *)
let addNodeToFrontOfList headNode myNode =
    match myNode with
      EmptyMutyList -> raise (MutyListError "Invalid Node Given as arg")  |
      NonemptyMutyList (a, b) -> match headNode with   
                                    NonemptyMutyList (x,y) -> b := headNode |
                                    _ -> raise (MutyListError "Invalid Node Given as arg") ;;
                                  






(* MUTY LIST MAKE. Return a NONEMPTY MUTY LIST with KEY and NEXT. *)

let mutyListMake key next =
  NonemptyMutyList (key, ref next) ;;








(* MUTY LIST WRITE. Write the KEY's of NODES, terminated by blanks, and ending
   with a newline. We write each KEY using FORMAT. *)

let mutyListWrite format nodes =
  let rec mutyListWriting nodes =
    match nodes with EmptyMutyList ->
           Printf.printf "\n" ;
           () |
         NonemptyMutyList (key, next) ->
           Printf.printf format key ;
           Printf.printf " " ;
           mutyListWriting (! next)
  in match nodes
     with EmptyMutyList ->
            raise (MutyListError "Missing head node.") |
          NonemptyMutyList (_, next) ->
            mutyListWriting (! next) ;;











(* MUTY LIST DELETE. Delete the first NONEMPTY MUTY LIST node that contains KEY
   from a MUTY LIST whose head is HEAD. Return (). *)

let mutyListDelete head key =
  let rec mutyListDeleting left right =
    match left with 
        EmptyMutyList -> () |
        NonemptyMutyList (leftKey, leftNext) ->
           if key = leftKey
           then match (! leftNext) with 
                    EmptyMutyList -> leftNext := EmptyMutyList |
                    NonemptyMutyList (_, rightNext) -> leftNext := (! rightNext)
           else mutyListDeleting right (! leftNext)
  in match head with 
                EmptyMutyList -> raise (MutyListError "Missing head node.") |
                NonemptyMutyList (_, right) -> mutyListDeleting head (! right) ;;



(*
(*the function should be able to check if it's case 1 deletion scenario or case 2 deletion scenario and do this by itself *)
let delete_this_node n =
    match !n with
        EmptyMutyList -> 
*)








(* Tests. We'll start by making KEYS, a MUTY LIST of STRINGs. The first node is
   a head node, with "" as its key. *)

let keys =
  (mutyListMake ""
    (mutyListMake "a"
      (mutyListMake "b"
        (mutyListMake "c"
          EmptyMutyList)))) ;;












(* Write KEYS, delete a node from KEYS, write KEYS again after the deletion,
   and repeat until KEYS is empty. Comments show what will be written. *)

mutyListWrite "%s" keys ;;   (* a b c *)

mutyListDelete keys "c" ;;

mutyListWrite "%s" keys ;;   (* a b *)

mutyListDelete keys "b" ;;

mutyListWrite "%s" keys ;;   (* a *)

mutyListDelete keys "a" ;;

mutyListWrite "%s" keys ;;   (*  *)











