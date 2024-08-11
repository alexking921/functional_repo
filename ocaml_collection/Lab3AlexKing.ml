(* this construction is saying EITHER the 'type' is an empty tree OR...it's a node with a left subtree and rightsubtree*) 
type 'key bst = BstEmpty | 
                BstNode of 'key * 'key bst * 'key bst ;;


(*this function finds the 'key' value in the tree you passed to it *)
let bstIsIn tree key =
    let rec isInning subtree = (*pass the 'tree' to this helper function where it's then referred to as 'subtree' *)
        match subtree
        with    BstEmpty -> false | (*if the subtree matches with BstEmpty, then return false bc we know the key isn't in tree *)
                BstNode otherKey leftSubtree rightSubtree -> if key < otherKey (*scenario 1: the targetKey is LESS THAN the current nodes key:  *)(*if the subtree matches with the BstNode then we know it's not empty and therefore 3 diff scenarios are possible: *)
                                                            then isInning leftSubtree (*scenario 1: this means the targetKey could only be in the LEFT subtree, hence why we pass the leftSubtree to the isInning method *)
                                                            else if key > otherKey(*scenario 2: the target key is GREATER THAN the current nodes key,  *)
                                                                then isInning rightSubtree(*Scenario 2: therefore the targetKey could only be in the RIGHT subtree, so we pass the rightSubtree to isInning  *)
                                                                else true (*Scenario 3: this is the GOAL scenario where targetKey = currentKey and we dont need a conditional to test this case because it's the last option and the code would only get to this point IF they were equal *)
    in isInning tree;;
                
(*general process of delete function
1. first FIND the key that is to be deleted and handle the case where the key is NOT in the tree in the first place
2.  Once found, Use match statements to look at the node and figure out which of 4 main cases you're dealing with (childless node, empty left node, empty right node, node with both left and right subtrees (the hardest case))
3. Then call the appropriate helper fucntion based on which of those match cases were satisfied*)
