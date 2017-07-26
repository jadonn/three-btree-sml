(* setup a seed for generating random numbers *)
val seed=let
  val m=Date.minute(Date.fromTimeLocal(Time.now()))
  val s=Date.second(Date.fromTimeLocal(Time.now()))
  in Random.rand(m,s)
end;


(* Utility function for generating a random integer; used when generating the initial tree *)
fun getRandInt(n)=Random.randRange(0,n) seed;

(* Data type for a normal binary tree with two children *)
datatype oneBTree = oneEmpty | oneNode of int * oneBTree * oneBTree;
(* Data type for the first level of three-children nodes *)
datatype twoBTree = twoEmpty | twoNode of int * twoBTree * oneBTree * twoBTree;
(* Data type for the top level of three-children nodes *)
datatype threeBTree = threeEmpty | threeNode of int * threeBTree * twoBTree * threeBTree;

(*
    Add a single node of type oneNode
    @param value to add, node of oneBTree to add value to
    @return a new node of oneBTree or the passed tree with the new node attached
*)
fun AddOneNode (i:int, oneEmpty) = oneNode(i, oneEmpty, oneEmpty) |
    AddOneNode(i:int, oneNode(j, left, right)) =  
        if i = j then oneNode(i, left, right)  
        else if i < j then oneNode(j, AddOneNode(i, left), right) 
        else oneNode(j, left, AddOneNode(i, right));
    

(* 
    Add a single node of type twoNode
    Makes use of AddOneNode to add the middle data member of type oneNode
    @param value to add, node of twoBTree to add value to
    @return a new node of twoBTree or the given tree with the new node attached
*)
fun AddTwoNode(0, twoEmpty) = twoEmpty | AddTwoNode(newValue:int, twoEmpty) = twoNode(newValue, twoEmpty, oneEmpty, twoEmpty) | 
    AddTwoNode(newValue:int, twoNode(value, twoEmpty, middle, right)) = twoNode(value, AddTwoNode(newValue, twoEmpty), middle, right) |
    AddTwoNode(newValue:int, twoNode(value, left, oneEmpty, right)) = twoNode(value, left, AddOneNode(newValue, oneEmpty), right) |
    AddTwoNode(newValue:int, twoNode(value, left, middle, twoEmpty)) = twoNode(value, left, middle, AddTwoNode(newValue, twoEmpty)) |
    AddTwoNode(newValue:int, twoNode(value, left, middle, right)) = 
    if newValue = value then twoNode(newValue, left, middle, right)
    else if newValue < value then twoNode(value, AddTwoNode(newValue, left), middle, right)
    else if newValue > getTwoNodeValue(right) then twoNode(value, left, middle, AddTwoNode(newValue, right))
    else twoNode(value, left, AddOneNode(newValue, middle), right);

(* 
    Add a single node of type threeNode
    Makes use of AddTwoNode to add the middle data member of type twoNode
    @param value to add, node of threeBTree to add value to
    @return a new node of threeBTree, or the passed in threeBTree with a new node attached
*)
fun AddThreeNode(0, threeEmpty) = threeEmpty | 
    AddThreeNode(newValue:int, threeEmpty) = 
        threeNode(newValue, threeEmpty, twoEmpty, threeEmpty) | 
    AddThreeNode(newValue:int, threeNode(value, threeEmpty, middle, right)) = 
        threeNode(value, AddThreeNode(newValue, threeEmpty), middle, right) |
    AddThreeNode(newValue:int, threeNode(value, left, middle, threeEmpty)) =
        threeNode(value, left, middle, AddThreeNode(newValue, threeEmpty)) |
    AddThreeNode(newValue:int, threeNode(value, left, twoEmpty, right)) =
        threeNode(value, left, AddTwoNode(newValue, twoEmpty), right) |
    AddThreeNode(newValue:int, threeNode(value, left, middle, right)) =
        if newValue = value then threeNode(value, left, middle, right)
        else if newValue < value then threeNode(value, AddThreeNode(newValue, left), middle, right)
        else if newValue > getThreeNodeValue(right) then threeNode(value, left, middle, AddThreeNode(newValue, right))
        else threeNode(value, left, AddTwoNode(newValue, middle), right);

(*
    Add n nodes with randomly generated values from 0 to 100 to a oneBTree
    @param number of nodes to generate, the oneBTree to add the nodes to
    @return a new oneBTree with the nodes or the passed in oneBTree with n additional nodes
*)
fun AddNodes(0, oneEmpty) = oneEmpty | AddNodes(0, oneNode(value, left, right)) = oneNode(value, left, right) | AddNodes(num:int, oneEmpty) = AddNodes(num - 1, AddOneNode(getRandInt(100), oneEmpty)) |
    AddNodes(num:int, oneNode(value, left, right)) =
        if num > 0 then AddNodes((num - 1),AddOneNode(getRandInt(100), oneNode(value, left, right)))
        else AddNodes(num, oneNode(value, left, right));

(*
    Add n nodes with randomly generated values from 0 to 100 to a twoBTree
    @param number of nodes to generate, the twoBTree to add the nodes to
    @return a new twoBTree with the nodes or the passed in twoBTree with n additional nodes
*)
fun AddTwoNodes(0, twoEmpty) = twoEmpty | 
    AddTwoNodes(0, twoNode(value, left, middle, right)) = twoNode(value, left, middle, right) | 
    AddTwoNodes(num:int, twoEmpty) = AddTwoNodes(num - 1, AddTwoNode(getRandInt(100), twoEmpty)) | 
    AddTwoNodes(num:int, twoNode(value, left, middle, right)) =
        if num > 0 then AddTwoNodes((num - 1), AddTwoNode(getRandInt(100), twoNode(value, left, middle, right)))
        else AddTwoNodes(num, twoNode(value, left, middle, right));

(*
    Add n nodes with randomly generated values from 0 to 100 to a threeBTree
    @param number of nodes to generate, the threeBTree to add the nodes to
    @return a new threeBTree with the nodes or the passed in twoBTree with n additional nodes
*)
fun AddThreeNodes(0, threeEmpty) = threeEmpty |
    AddThreeNodes(0, threeNode(value, left, middle, right)) = threeNode(value, left, middle, right) |
    AddThreeNodes(num:int, threeEmpty) = AddThreeNodes(num - 1, AddThreeNode(getRandInt(100), threeEmpty)) |
    AddThreeNodes(num:int, threeNode(value, left, middle, right)) = 
        if num > 0 then AddThreeNodes(num - 1, AddThreeNode(getRandInt(100), threeNode(value, left, middle, right)))
        else AddThreeNodes(num, threeNode(value, left, middle, right));


(*
    Helper function for getting the value of the given oneNode
    @param oneNode
    @return value of the oneNode
*)
fun getOneNodeValue(oneEmpty) = 0 |
    getOneNodeValue(oneNode(value, left, right)) = value;

(*
    Helper function for getting the value of the given twoNode
    @param twoNode
    @return value of the twoNode
*)
fun getTwoNodeValue(twoEmpty) = 0 |
    getTwoNodeValue(twoNode(value, left, middle, right)) = value;

(*
    Helper function for getting the value of the given ThreeNode
    @param threeNode
    @return value of the threeNode
*)
fun getThreeNodeValue(threeEmpty) = 0 |
    getThreeNodeValue(threeNode(value, left, middle, right)) = value;

(*
    Wrapper function for removing need for user to supply empty initial search path string
    @param value to be searched for, oneBTree to search
    @return string of the route to the found node or that the node cannot be found
*)
fun findOneNode(search:int, oneNode(value, left, right)) = 
    findOneHelper(search, oneNode(value, left, right), "");    

(*
    Helper function that traverses a oneBTree to find a node of the given search value
    @param value to search for, oneBTree to be searched, empty initial path for recording path taken
    @return string of the route to the node or that the node cannot be found
*)    
fun findOneHelper(search:int, oneNode(value, left, right), path:string) =
    if search = value then print (path ^ "(" ^ Int.toString(value) ^ ")\n")
    else if search < value then findOneHelper(search, left, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else if search > value then findOneHelper(search, right, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else print "NOT FOUND\n" | findOneHelper(search, oneEmpty, path) = print (path ^ "NOTFOUND\n");

(*
    Wrapper function for removing need for user to supply empty initial search path string
    @param value to be searched for, twoBTree to search
    @return string of the route to the found node or that the node cannot be found
*)    
fun findTwoNode(search:int, twoNode(value, left, middle, right)) =
    findTwoHelper(search:int, twoNode(value, left, middle, right), "")

(*
    Helper function that traverses a twoBTree to find a node of the given search value
    @param value to search for, twoBTree to be searched, empty initial path for recording path taken
    @return string of the route to the node or that the node cannot be found
*)     
fun findTwoHelper(search:int, twoNode(value, left, middle, right), path:string) =
    if search = value then print (path ^ "(" ^ Int.toString(value) ^ ")\n")
    else if search < value then findTwoHelper(search, left, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else if search > getTwoNodeValue(right) then findTwoHelper(search, right, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else findOneHelper(search, middle, (path ^ "(" ^ Int.toString(value) ^ ")\n")) | 
    findTwoHelper(search, twoEmpty, path) = print (path ^ "NOTFOUND\n");

(*
    Wrapper function for removing need for user to supply empty initial search path string
    @param value to be searched for, threeBTree to search
    @return string of the route to the found node or that the node cannot be found
*)    
fun findThreeNode(search:int, threeNode(value, left, middle, right)) =
    findThreeHelper(search:int, threeNode(value, left, middle, right), "");

(*
    Helper function that traverses a threeBTree to find a node of the given search value
    @param value to search for, threeBTree to be searched, empty initial path for recording path taken
    @return string of the route to the node or that the node cannot be found
*)     
fun findThreeHelper(search:int, threeNode(value, left, middle, right), path:string) =
    if search = value then print (path ^ "(" ^ Int.toString(value) ^ ")\n")
    else if search < value then findThreeHelper(search, left, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else if search > getThreeNodeValue(right) then findThreeHelper(search, right, (path ^ "(" ^ Int.toString(value) ^ ")\n"))
    else findTwoHelper(search, middle, (path ^ "(" ^ Int.toString(value) ^ ")\n")) |
    findThreeHelper(search, threeEmpty, path) = print (path ^ "NOTFOUND\n");

(*
    Wrapper function for printing out the contents of a oneBTree without requiring user to supply
    the base string used to start the path
    @param oneBTree
    @return multi-line string output with each line containing a single node
*)
fun printOneTree(oneEmpty) = () |
    printOneTree(oneNode(value, left, right)) = printOneTreeHelper(oneNode(value, left, right), "");

(*
    Helper function for printing out the contents of a oneBTree without requiring user to supply
    the base string used to start the path
    @param oneBTree, path containing all the nodes and their children
    @return multi-line string output with each line containing a single node
*)    
fun printOneTreeHelper(oneEmpty, path:string) = print path |
    printOneTreeHelper(oneNode(value, left, right), path:string) = (print (path ^ "(" ^ Int.toString(value) ^ "," ^ Int.toString(getOneNodeValue(left)) ^ "," ^ Int.toString(getOneNodeValue(right)) ^ ")\n"); printOneTreeHelper(left, "");  printOneTreeHelper(right, ""));

(*
    Wrapper function for printing out the contents of a twoBTree without requiring user to supply
    the base string used to start the path
    @param twoBTree
    @return multi-line string output with each line containing a single node
*)    
fun printTwoTree(twoEmpty) = () |
    printTwoTree(twoNode(value, left, middle, right)) = printTwoTreeHelper(twoNode(value, left, middle, right), "");

(*
    Helper function for printing out the contents of a twoBTree without requiring user to supply
    the base string used to start the path
    @param twoBTree, path containing all the nodes and their children
    @return multi-line string output with each line containing a single node
*)      
fun printTwoTreeHelper(twoEmpty, path:string) = print path |
    printTwoTreeHelper(twoNode(value, left, middle, right), path:string) = (print (path ^ "(" ^ Int.toString(value) ^ "," ^ Int.toString(getTwoNodeValue(left)) ^ "," ^ Int.toString(getOneNodeValue(middle)) ^ "," ^ Int.toString(getTwoNodeValue(right)) ^ ")\n"); printTwoTreeHelper(left, ""); printOneTreeHelper(middle, ""); printTwoTreeHelper(right, ""));

(*
    Wrapper function for printing out the contents of a threeBTree without requiring user to supply
    the base string used to start the path
    @param threeBTree
    @return multi-line string output with each line containing a single node
*)    
fun printThreeTree(threeEmpty) = () |
    printThreeTree(threeNode(value, left, middle, right)) = printThreeTreeHelper(threeNode(value, left, middle, right), "");

(*
    Helper function for printing out the contents of a threeBTree without requiring user to supply
    the base string used to start the path
    @param threeBTree, path containing all the nodes and their children
    @return multi-line string output with each line containing a single node
*)      
fun printThreeTreeHelper(threeEmpty, path:string) = print path |
    printThreeTreeHelper(threeNode(value, left, middle, right), path:string) = (print (path ^ "(" ^ Int.toString(value) ^ "," ^ Int.toString(getThreeNodeValue(left)) ^ "," ^ Int.toString(getTwoNodeValue(middle)) ^ "," ^ Int.toString(getThreeNodeValue(right)) ^ ")\n"); printThreeTreeHelper(left, ""); printTwoTreeHelper(middle, ""); printThreeTreeHelper(right, ""));

(*
    Wrapper function for generating a new threeBTree without having to pass a threeBTree
    @param number of nodes to generate on new three
    @return new threeBTree with n nodes
*)
fun generateTree(0) = threeEmpty |
    generateTree(numNodes:int) = AddThreeNodes(numNodes,  threeEmpty);