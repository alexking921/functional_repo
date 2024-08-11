(*Lab 4 Alex King (no partner) *)

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
        BstNode(otherKey, BstEmpty, BstEmpty) -> if targetKey = otherKey
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









(*TEST RESULTS:*)

─( 16:57:15 )─< command 22 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 149 ;;
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstEmpty, BstNode (997, BstEmpty, BstEmpty))))
─( 16:57:21 )─< command 23 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 997 ;;
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
─( 16:57:31 )─< command 24 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 86 ;;
val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (74, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
─( 16:58:10 )─< command 25 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 100 ;;
val t : int bst =
  BstNode (74, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
─( 16:58:37 )─< command 26 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 70 ;;
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
─( 16:58:56 )─< command 27 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 137 ;;
val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (212, BstEmpty, BstEmpty))
─( 16:59:13 )─< command 28 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 53  ;;
val t : int bst = BstNode (74, BstEmpty, BstNode (212, BstEmpty, BstEmpty))
─( 16:59:23 )─< command 29 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 212 ;;
val t : int bst = BstNode (74, BstEmpty, BstEmpty)
─( 16:59:53 )─< command 30 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let t = bstDelete t 74  ;;
val t : int bst = BstEmpty
─( 17:00:09 )─< command 31 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # 





