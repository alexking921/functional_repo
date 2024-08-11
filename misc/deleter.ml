(*
  DELETER.ML. Show how to delete nodes from a singly linked mutable list.

    James Moen
    31 Oct 21

  This was an example shown in lecture last week, with slight changes.
*)








(* MUTY LIST. A singly linked linear list of nodes that contain BASEs, linked
   together by mutable pointers. Each MUTY LIST has a head node. *)

type 'base mutyList =
  EmptyMutyList |
  NonemptyMutyList of 'base * 'base mutyList ref ;;






(* MUTY LIST ERROR. Raised when something bad happens. The STRING tells us what
   the bad thing was. *)

exception MutyListError of string ;;





(* MUTY LIST MAKE. Return a NONEMPTY MUTY LIST with KEY and NEXT. *)

let mutyListMake key next =
  NonemptyMutyList (key, ref next) ;;








(* MUTY LIST WRITE. Write the KEY's of NODES, terminated by blanks, and ending
   with a newline. We write each KEY using FORMAT. *)

let mutyListWrite format nodes =
  let rec mutyListWriting nodes =
    match nodes
    with EmptyMutyList ->
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
    match left
    with EmptyMutyList ->
           () |
         NonemptyMutyList (leftKey, leftNext) ->
           if key = leftKey
           then match (! leftNext)
                with EmptyMutyList ->
                       leftNext := EmptyMutyList |
                     NonemptyMutyList (_, rightNext) ->
                       leftNext := (! rightNext)
           else mutyListDeleting right (! leftNext)
  in match head
     with EmptyMutyList ->
            raise (MutyListError "Missing head node.") |
          NonemptyMutyList (_, right) ->
            mutyListDeleting head (! right) ;;












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











