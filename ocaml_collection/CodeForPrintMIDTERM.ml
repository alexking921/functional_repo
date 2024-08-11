
(*LAB 1 functions: *)
open List;;

let rec howMany e l =
	if (l = [])
	then 0
	else if ((hd l) = e)
	then (1 + (howMany e (tl l)))
	else (howMany e (tl l))
	;;

let rec delete e l =
	if (l = [])
	then l
	else if (hd l = e)
	then delete e (tl l)
	else ((hd l) :: delete e (tl l));;

let rec length l =
	if l = []
	then 0.0
	else (1.0 +. (length (tl l)));;

let rec sum l =
	if l = []
	then 0.0
	else ((hd l) +. (sum (tl l)));;

let rec mean l =
	if l = []
	then 0.0
	else ((sum l) /. (length l));;



















(*** LAB 2 functions*)
let num = fst ;;
let den = snd ;;

let rec gcd i j =
    if i <> 0
    then if j > i
        then gcd i (j - i)
        else gcd (i - j) j
    else j;;

(*rat *)
let rat n d =
    ((n / (gcd n d)), (d / (gcd n d)));;

(*ratAdd *)
let ratAdd a b =
rat ((num a * den b) + (den a * num b)) (den a * den b) ;;

(*ratMul *)
let ratMul a b =
rat ((num a * num b) (den a * den b));;

(*ratDiv *)
let ratDiv a b =
rat (num a * den b) (den a * num b);;

(*ratGt     boolean function*) 
let ratGt a b =
    if (num a * den b) > (den a * num b)
    then true
    else false
    ;;

(*EULER FUNCTION*)
let epsilon = (1,100000);;

let euler =
    let rec eulering c s t =
        if ratGt t epsilon
        then let s' = ratAdd s t
            in let t' = ratDiv t c
                in let c' = ratAdd c (1,1)
                    in eulering c' s' t'
        else s
in eulering (1,1) (0,0) (1,1) ;;

















(*Lab 3 Alex King (no partner) *)
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;


exception BadEmptyBst ;;
let rec bstMaxKey tree =
  match tree
  with BstEmpty -> raise BadEmptyBst |
       BstNode(key, _, BstEmpty) -> key |
       BstNode(_, _, rightSubtree) -> bstMaxKey rightSubtree ;;



let bstInsert tree key =
  let rec inserting subtree =
    match subtree
    with BstEmpty -> BstNode(key, BstEmpty, BstEmpty) |
         BstNode(otherKey, leftSubtree, rightSubtree) ->
           if key < otherKey
           then BstNode(otherKey, inserting leftSubtree, rightSubtree)
           else if key > otherKey
                then BstNode(otherKey, leftSubtree, inserting rightSubtree)
                else subtree
  in inserting tree ;;


let bstIsIn key tree =
  let rec isInning subtree =
    match subtree
    with BstEmpty -> false |
         BstNode(otherKey, leftSubtree, rightSubtree) ->
           if key < otherKey
           then isInning leftSubtree
           else if key > otherKey
                then isInning rightSubtree
                else true
  in isInning tree ;;




let t = BstEmpty        ;;
let t = bstInsert t 100 ;;
let t = bstInsert t 70  ;;
let t = bstInsert t 137 ;;
let t = bstInsert t 53  ;;
let t = bstInsert t 86  ;;
let t = bstInsert t 74  ;;
let t = bstInsert t 212 ;;
let t = bstInsert t 149 ;;
let t = bstInsert t 997 ;;




let bstDelete tree targetKey =
    if bstIsIn targetKey tree = false 
    then tree
    else let rec deleting subtree targetKey =  
        match subtree                                     
        with
        BstEmpty -> BstEmpty |
        BstNode(otherKey, BstEmpty, BstEmpty) -> 
                        if targetKey = otherKey
                        then BstEmpty
                        else subtree |
        BstNode(otherKey, BstEmpty, rightSubtree) -> 
                        if targetKey > otherKey
                        then BstNode(otherKey, BstEmpty, (deleting rightSubtree targetKey))
                        else if targetKey < otherKey
                        then subtree
                        else rightSubtree |
        BstNode(otherKey, leftSubtree, BstEmpty) -> 
                        if targetKey < otherKey
                        then BstNode(otherKey, (deleting leftSubtree targetKey), BstEmpty)
                        else if targetKey > otherKey
                        then subtree
                        else leftSubtree |  
        BstNode(otherKey, leftSubtree, rightSubtree) -> if targetKey < otherKey
                        then BstNode(otherKey, (deleting leftSubtree targetKey), rightSubtree) 
                        else if targetKey > otherKey 
                        then BstNode(otherKey, leftSubtree, (deleting rightSubtree targetKey))
                        else BstNode((bstMaxKey leftSubtree), (deleting leftSubtree (bstMaxKey leftSubtree)), rightSubtree)
        in deleting tree targetKey;;


