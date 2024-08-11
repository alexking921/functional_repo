
let rec fac n =
    if n > 1
    then n * fac(n-1)
    else 1;;


let makeFac l = 
    let t = Array.make l (-1)
    in let rec facing n =
        if t.(n) >= 0
        then t.(n)
        else t.(n) <- (match n with
                        0 -> 1 |
                        1 -> 1 |
                        _ -> n * facing(n-1) ; 
                                            t.(n))
    in facing;;

let memoFac = makeFac 20;;

(*example call for the factorial 7! *)

memoFac 7;;






























let epsilon = 0.00001;;
let squirt a =
    let rec squirting g h =
    if abs_float(g -.h) >= epsilon
    then let g' = (g +. h)/. 2.0
        in let h' = a/.g'
            in squirting g' h'
    else g
in squirting 1.0 a ;;

exception EmptyList;;

let range low high etc = 
    let rec ranging index = 
        if index <= high
        then (etc index;
            ranging (index + 1))
        else ()
    in ranging low;;

let pairs low high etc = 
    range low high
    (fun left ->
        range low high
        (fun right -> 
            etc left right));;


let opposite predicate = (fun thing -> not (predicate thing));;

let map func things =
    let rec mapping things = 
    match things
    with [] -> [] |
        firstThing::otherThings -> (func firstThing)::mapping otherThings
    in mapping things;; 


let filter predicate things = 
    let rec filtering things = 
        match things
        with [] -> [] |
            firstThing::otherThings ->
            	if predicate firstThing 
    	      	then firstThing::(filtering otherThings)      
		else filtering otherThings 
    in filtering things;;




let rightReduce func things thing = 
    let rec rightReducing things = 
        match things
        with [] -> thing | 
        firstThing::otherThings -> func firstThing (rightReducing otherThings)
    in rightReducing things;;

let rec last things = 
    match things
    with [] -> raise EmptyList |
    [thing] -> thing | 
    _::otherThings ->last otherThings;; 

let rec butlast things = 
    match things
    with [] -> raise EmptyList |
    [_] -> [] | 
    firstThing::otherThings -> firstThing::butlast otherThings;;

let leftReduce func things thing = 
    let rec leftReducing things =
        if things = [] 
        then thing
        else func (leftReducing (butlast things)) (last things)
    in leftReducing things;;

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


let evaluate prop pairs = 
    let rec evaluating prop = 
    match prop with 
    False -> false |
    True -> true | 
    Var name -> alGet name pairs | 
    Not right -> not (evaluating right) |
    And (left, right) -> (evaluating left) && (evaluating right) |
    Or (left, right) -> (evaluating left) || (evaluating right) |
    Imply (left, right) -> not (evaluating left) || (evaluating right) |
    Equiv (left, right) -> (evaluating left) = (evaluating right)
in evaluating prop;;

let generateBools etc n = 
    let rec generating bools n =
        match n with 
		0 -> etc bools |  
       	 _ -> generating (false::bools)(n-1) ; 
            		generating (true::bools)(n-1) 
    in generating [] n;;  


let generatePairs etc names = 
    let rec generating names pairs = 
        match names with
 		[] -> etc pairs | 
        	name::otherNames -> 
			generating otherNames (alPut pairs name false) ;
      			generating otherNames (alPut pairs name true) 
    in generating names [] ;;



let generateAndTestPairs etc names = 
    let rec generating names pairs = 
        match names with 
		[] -> etc pairs | 
        	name::otherNames -> 
        		generating otherNames (alPut pairs name false) &&
        		generating otherNames (alPut pairs name true) 
    in generating names [];;
let isMember name names = 
    let rec membering names = 
        match names with 
		[] -> false | 
        	otherName::otherNames -> if name = otherName
            				then true  
            				else membering otherNames 
    in membering names;;


let union leftNames rightNames = 
    let rec unioning bothNames leftNames = 
        match leftNames with 
		[] -> bothNames | 
        	leftName::otherLeftNames ->
		   if isMember leftName bothNames 
		   then unioning bothNames otherLeftNames 
		   else unioning (leftName::bothNames) otherLeftNames 
    in unioning rightNames leftNames;; 
let rec names prop = 
  match prop with 
    True | False -> [] | 
    Var name  -> [name] | 
    Not (a)   -> names a | 
    And(a,b)   -> union (names a) (names b)| 
    Or(a,b)    -> union (names a) (names b)| 
    Imply(a,b) -> union (names a) (names b)| 
    Equiv(a,b) -> union (names a) (names b);;

let isTautology prop = 
    generateAndTestPairs 
	(fun pairs -> evaluate prop pairs) 
	(names prop);;  











let makeStream this state next =
    ((this, state), next);;


let first ((this, _), _) =
    this;;

let rest ((this, state), next) =
    (next this state, next);;














let naturals = makeStream 0 () (fun this state -> (this+1, ()));;


let factorials = makeStream 1 1 (fun this state -> (this*state, state+1));;


let advance predicate stream =
    let rec advancing stream =
        if predicate (first stream)
        then stream  
        else advancing (rest stream)
    in advancing stream;;


let map func stream =
    makeStream 
(func (first stream)) 
(rest stream) 
(fun this state -> ((func (first state)), rest state));; 



let rec trim count stream =
    match count with
    0 -> stream |
    _ -> trim (count-1) (rest stream);;


let skip count stream =
    makeStream
        (first stream)
        (stream)
        (fun this state -> let state = trim count state
                    in (first state, state));;




let rec take count stream =
    match count with
    0 -> [] |
    _ -> first stream::take (count-1)(rest stream);;



let fac n =
    let f = ref 1   
    in let k = ref n
        in while (!k > 0)   
            do f:= !k * !f ;   
    			k := !k-1
		done; 
		!f;;  



(*
exception StackError;;

type ‘base StackList = 
    EmptyStack |
    NonemptyStack of ‘base * ‘base StackList;;   

type ‘base stackOperation = 
    isEmpty |
    Peek | 
    Pop |
    Push of ‘base;;

type ‘base stackResult =
    BoolResult of bool | 
    BaseResult of ‘base | 
    NoResult;;

type ‘base stack = 
    ‘base stackOperation -> ‘base stackResult;;

let top = ref Empty Stack in
        let isEmpty() =
            BoolResult(!top = EmptyStack) in 
        let peek() = 
            match !top with
                EmptyStack -> raise StackError |
                NonemptyStack(first,_) -> BaseResult first in
        let pop() = 
            match !top with
                EmptyStack -> raise StackError |
                NonemptyStack(_,rest) -> top:=rest; NoResult in 
        let push base =
            top:=Nonempty(base, !top); NoResult in 
        let dispatch operation =
            match operation with
                IsEmpty -> isEmpty() |
                Peek -> peek() |
                Pop -> pop() |
                Push base -> push base
        in dispatch;; 



*)
















exception StackError of string ;;

(* STACK LIST. A singly linked linear list containing elements of type BASE. *)
type 'base stackList =
  EmptyStack |
  NonemptyStack of 'base * 'base stackList ;;



(* STACK OPERATION. Operations on STACKs whose elements have type BASE. *)
type 'base stackOperation =
  IsEmpty |
  Peek |
  Pop |
  Push of 'base ;;



(* STACK RESULT. The results of operations on STACKs whose elements have type BASE. *)
type 'base stackResult =
  BoolResult of bool |
  BaseResult of 'base |
  NoResult ;;



(* STACK. A STACK whose elements have type BASE. It's actually a function that  maps STACK OPERATIONs to STACK RESULTs, as the arrow -> shows. *)
type 'base stack =
  	'base stackOperation -> 'base stackResult ;;








(*

let makeStack () =
  let top = ref EmptyStack
  in let isEmpty () = 
    BoolResult (! top = EmptyStack)
  in let peek () = 
    match !top with 
		EmptyStack -> raise (StackError "Can't PEEK into an empty stack.") |
         	NonemptyStack (atTop, _) -> BaseResult atTop
  in let pop () = 
    match !top with 
	EmptyStack -> raise (StackError "Can't POP an empty stack.") |
	NonemptyStack (_, underTop) -> top := underTop ;  
						     NoResult
  in let push base = 
    top := NonemptyStack (base, ! top) ; 
    	    NoResult
  in let dispatch operation = 
    match operation with 
	IsEmpty -> isEmpty () |
     	Peek -> peek () |
      	Pop -> pop () |
    	Push base -> push base
  in dispatch ;;


let s = makeStack () ;;


*)












type 'base mutyList =
  EmptyMutyList |
  NonemptyMutyList of 'base * 'base mutyList ref ;;

    



exception Impossible;; 

let mutyListDelete head key = 
    let rec mutyListDeleting left right =
        match right with 
		EmptyMutyList -> () | 
    		NonemptyMutyList(rightKey, rightNext) ->
            		if key = rightKey 
            		then match left with 
			EmptyMutyList -> raise Impossible |
			NonemptyMutyList(_, leftNext) -> 
                    	leftNext:=!rightNext    
            		else mutyListDeleting right (!rightNext)  
    in match head with 
	EmptyMutyList -> raise Impossible |
        NonemptyMutyList(_, headNext) -> mutyListDeleting head (!headNext);;










exception MutyListError of string ;;


let mutyListMake key next =
  NonemptyMutyList (key, ref next) ;;










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





let mutyListDelete head key =
  let rec mutyListDeleting left right =
    match left with 
		EmptyMutyList -> () | 
         	NonemptyMutyList (leftKey, leftNext) ->
           				if key = leftKey 
					then 
						match (! leftNext) with 
						   EmptyMutyList -> leftNext := EmptyMutyList |
                     			   NonemptyMutyList (_, rightNext) -> leftNext := (! rightNext)
           				else mutyListDeleting right (! leftNext)
  in match head with
	EmptyMutyList -> raise (MutyListError "Missing head node.") | 
      	NonemptyMutyList (_, right) -> mutyListDeleting head (! right) ;;


let keys =
  (mutyListMake ""
    (mutyListMake "a"
      (mutyListMake "b"
        (mutyListMake "c"
          EmptyMutyList)))) ;;
























let rec fib n =  
    match n
    with 0 -> 0 |
    1-> 1 |
    _ -> fib (n-2) + fib (n-1);;





(*
let time s f x = 
    let t0 = Sys.time()
    		in let y = f x 
        	in let t1 = Sys.time()
            	in Printf.printf “%s %f seconds \n” s(t1-.t0);
			
                y;;
*)



let makeFib l =
    let t = Array.make l (-1) 
    in let rec fibbing n = 
	if t.(n) >= 0 
        then t.(n)  
        else (t.(n) <- 
            (match n with  
            		0 -> 0 | 	
                	1 -> 1 |	
                	_ -> fibbing(n-2) + fibbing(n-1));
            			t.(n))
    in fibbing;; 




let fib1 = makeFib 50;; 






(*
let memoize f = 
    let t = ref []
        in let rec memoizing p x = 
            match p with
            [] -> let y = f x
                    in t := (x,y)::!t;  
                y |	   
            (x',y)::p' -> if x = x' 
                            then y 
                            else memoizing p' x
        in (fun x -> memoizing (!t) x);; 



let f = fib 3;;

let f' = memoize f;;
*)


let rec hang() = 
    hang();;



let undefined() = 
    0/0;
    ();;



type proposition =
    False |
    True |
    Undefined |
    Var of string |
    Not of proposition|
    And of proposition * proposition;;



exception EvaluationError;;






let evaluateEager proposition pairs = 
    let rec evaluating proposition = 
        match proposition 
        with False -> false |
            True -> true |
            Undefined -> raise EvaluationError |
            Var name -> alGet name pairs |
            Not right -> not (evaluating right) |
            And (left, right) -> 
                let left' = evaluating left
                in let right' = evaluating right
                in left' && right'

    in evaluating proposition;;



let evaluateLazy proposition pairs = 
    let rec evaluating proposition = 
        match proposition 
        with False -> false |
            True -> true |
            Undefined -> raise EvaluationError |
            Var name -> alGet name pairs |
            Not right -> not (evaluating right) |
            And (left, right) -> 
                (evaluating left) && (evaluating right)
    in evaluating proposition;;







let mul x y =
    if x=0
    then 0
    else if y=0
        then 0
        else x*y;;


let anonymous_mul1 x y = 
    if x()=0
    then 0
    else if y()=0
        then 0
        else x()*y();;




let anonymous_mul2 x y = 
    let x'=x()
    in if x'=0
        then 0
        else let y'=y()
        in if y'=0
            then 0
            else x'*y';;







(*
type 'base delayed = 
    Value of 'base |  
    Function of (() -> 'base) ;; 
    

type 'base future = 'base delayed ref;;


let delay func =
    ref Function (func);;


let x =
delay (fun()->fib 46);; 


let demand future =
    match !future
    with Value(result) -> result 
    Function(func) ->   
        let temp = func() 
        in future := Value temp; 
        temp;; 


let future_mul x y =
    if demand x=0
    then 0
    else if demand y=0
        then 0
        else demand x * demand y;; 



*)
