(* Alex King (no-partner)
  CSci 2041 Lab Assignment 7
    James Moen
    25 Oct 21
  It's worth 35 points.
*)
(* MUTY QUEUE. A mutable queue of BASEs, as a circular doubly linked list. The
   type doesn't say that the list is circular or doubly linked. That's done by
   the functions that manipulate MUTY QUEUEs. All those functions must work in
   O(1) time. *)
type 'base mutyQueue =
  MutyQueueNode of
   'base *
   'base mutyQueue ref *
   'base mutyQueue ref ;;






(* 
  Put your code for MUTY QUEUE MAKE, MUTY QUEUE EMPTY, MUTY QUEUE ENQUEUE and
  MUTY QUEUE DEQUEUE here. *)
let mutyQueueMake s =
  let rec h = MutyQueueNode (s, ref h, ref h) 
  in h;;



                      
(*checking if pointers b and c are pointing to their own node 'q'...NOT 'a' *)
let mutyQueueEmpty q = 
  match q with
    MutyQueueNode (a, b, c) -> if !b == q && !c == q then true
                                  else false;;



let mutyQueueEnqueue q e =
  match q with
    MutyQueueNode (a, b, c) -> match !b with 
                                MutyQueueNode (_, _, z) ->
                                            let newNode = MutyQueueNode (e, ref ! b, ref q) 
                                            in b := newNode; 
                                            z := newNode;;
    


(* we want to tell *)
let mutyQueueDequeue q = 
  if mutyQueueEmpty q then (match q with
                            MutyQueueNode (s, _, _) -> s)
  else match q with
    MutyQueueNode (a, headLeft, headRight) -> match !headRight with
                                      MutyQueueNode (x, _, right) -> match !right with
                                                                        MutyQueueNode (_, leftLeft, _) -> 
                                                                                                        leftLeft := q ; headRight := !right; x;;

(* Make a QUEUE whose sentinel is the empty string "" and test it. The comments
   say what each test should return, and how many points you get (if any) for
   successful tests. *)
let queue = mutyQueueMake "" ;;
(* 2 pt. MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>}) *)
mutyQueueEmpty queue ;;           (* 2 pt. true *)
mutyQueueDequeue queue ;;       (* 2 pt. "" *)
mutyQueueEnqueue queue "A" ;;     (* 1 pt. () *)
mutyQueueEmpty queue ;;           (* 2 pt. false *)
mutyQueueEnqueue queue "B" ;;     (* 1 pt. () *)
mutyQueueEnqueue queue "C" ;;     (* 1 pt. () *)
mutyQueueDequeue queue ;;         (* 5 pt. "A" *)
mutyQueueDequeue queue ;;         (* 5 pt. "B" *)
mutyQueueDequeue queue ;;         (* 5 pt. "C" *)
mutyQueueEmpty queue ;;           (* 2 pt. true *)
mutyQueueDequeue queue ;;         (* 5 pt. "" *)
mutyQueueDequeue queue ;;         (* 2 pt. "" *)





(* 

My Test Results:

# #use "alexkinglab7tests.ml";;
type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val mutyQueueMake : 'a -> 'a mutyQueue = <fun>
val mutyQueueEmpty : 'a mutyQueue -> bool = <fun>
val mutyQueueEnqueue : 'a mutyQueue -> 'a -> unit = <fun>
val mutyQueueDequeue : 'a mutyQueue -> 'a = <fun>
val queue : string mutyQueue =
  MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>})
  
- : bool = true
- : string = ""
- : unit = ()
- : bool = false
- : unit = ()
- : unit = ()
- : string = "A"
- : string = "B"
- : string = "C"
- : bool = true
- : string = ""
- : string = ""
# 

 *)


