(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment eight                   *)

module a8

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)

/// A datatype for representing a Binary Tree whose node elements are of
/// generic type.
type BinTree<'a> = Node of 'a BinTree * 'a * 'a BinTree
                 | Leaf

let lt3   = Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf)
let rt3   = Node (Leaf, 3, Node (Leaf, 2, Node (Leaf, 1, Leaf)))

/// Count the number of `Node`s in a tree.
let rec count = function
    | Leaf          -> 0
    | Node(lt,n,rt) -> count lt + count rt + 1;;

/// Count the number of `Node`s in a tree (using continuation function)
let rec countC t c =
    match t with
    | Leaf          -> c 0
    | Node(tl,_,tr) -> countC tl (fun vl -> countC tr (fun vr -> c (vl+vr+1)))
let _ = countC : BinTree<'a> -> (int -> 'b) -> 'b

/// Generate a big list using continuation functions (used in 8.3)
let rec bigListK n k =
    if n <= 0
    then k []
    else bigListK (n-1) (fun res -> 1::k(res))
let _ = bigListK : int -> ('a list -> int list) -> int list

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


(* Exercise 8.1 (HR 9.8) *)

/// Count the number of Nodes in given Binary Tree using an accumulator argument.
/// This function is not tail-recursive since it will have to first complete the
/// recursive call to one subtree before traversing the other.

let rec countA n t =
    match t with 
    | Leaf -> n
    | Node(lt, _, rt) -> countA(countA (n+1)lt)rt 
let _ = countA : int -> BinTree<'a>  -> int 

(* Exercise 8.2 (HR 9.9) *)

/// Count the number of Nodes in given Binary Tree using an accumulator
/// argument as well as a continuation function.
// t = bintree to be counted, acc = accumulator, cont = continuation function. 

let rec countAC t acc cont = 
    match t with
    | Leaf            -> cont acc
    | Node(lt, _, rt) -> countAC rt (acc+1)(fun a -> countAC lt a (fun a -> cont(a)))
let _ = countAC : BinTree<'a> -> int -> (int -> 'b) -> 'b

// Note use of helper function, making countAC use CountC's recursive structure, and thereby not being recursive itself.
let countAC' t n c = 
    match t with 
    | Leaf -> c n
    | Node(lt, _, rt) -> countC lt (fun vl -> countC rt (fun vr -> c (vl+vr+1+n)))
let _ = countAC' : BinTree<'a> -> int -> (int -> 'b) -> 'b

(* Exercise 8.3 (HR 9.10) *)

// Analysis of the StackOverflow:
(* 
It is as a rule of thumb, that a program is significantly less stack heavy, when using continuation passing style (CPS) in an implementation,
compared to recursion, tail recursion or an imperative programming style. This is due to CPS passing a function to 
continue or terminate a program, in opposition to extracting a value back (return). By having an implementation with a
continuation function we thereby eliminate the requirement of a call stack and a return statement, 
all the calculations happens in the end. However, when passing the 'bigListK' the parameter of '130000 id', we achieve a
stackoverflow, eventhough the implementation of 'bigListK' uses CPS. The first lesson to be learned here, is that,
eventhough we have a CPS implementation, there are sizes, that will cause a stackoverflow. Theorizing over the results,
of running 'bigListK' using different sizes of ints as parameters, it is clear, that the ints required for a stackoverflow,
are of rather large sizes, where a different implementation that does not use CPS, would probably result in a stackoverflow
for a relatively lower size. The very interesting part is, that if this implementation is passed a size smaller than 130000, 
e.g. 120000, we can achieve a execution shutdown, due to the higher expence of calculation in CPS, well before we reach
a size that causes stackoverflow. An implementation using tailrecursion with an accumilator, would in theory always reach 
a stack overflow before the expence of calculating becomes too much for the hardware to handle. 
*)

(* Exercise 8.4 (HR 9.11) *) 
// not implemented, the current implementation is "just" a starting point so far. 

/// Generate an unbalanced `BinTree` where the value of all nodes is the height
/// of the remainder of the tree and all right subtrees are leafs.
///
/// I.e. `leftTree 5 id` should generate the tree:
///
///           5
///          / \
///         4
///        / \
///       3
///      / \
///     2
///    / \
///   1
///  / \
///
let rec leftTreeNaive = function
    | n when n = 0 -> Leaf
    | n when n > 0 -> Node(leftTreeNaive (n-1), n, Leaf)
    | _            -> failwith "unsupported input"
let _ = leftTreeNaive : int -> BinTree<int>
let treeJonas = Node(Leaf, 1, Leaf)

let rec leftTree n c = 
    match n with 
    | n when n <= 0 -> c Leaf 
    | n when n > 0  -> leftTree (n-1) (fun a -> c(Node(a, n, Leaf)))
    | _             -> failwith "unsupported input"
let _ = leftTree : int -> (BinTree<int> -> 'a) -> 'a

/// Generate an unbalanced `BinTree` where the value of all nodes is the height
/// of the remainder of the tree and all left subtrees are leafs.
let rec rightTreeNaive = function
    | n when n = 0 -> Leaf
    | n when n > 0 -> Node(Leaf, n, rightTreeNaive (n-1))
    | _            -> failwith "unsupported input"
let _ = rightTreeNaive : int -> BinTree<int>

let rec rightTree n c = 
    match n with
    | n when n < 1  -> c Leaf
    | n when n >= 1 -> rightTree (n-1) (fun a -> c(Node(Leaf, n, a)))
    | _             -> failwith "unsupported input"
let _ = rightTree : int -> (BinTree<int> -> 'a) -> 'a

// 1: Use `leftTree` and `rightTree` functions to show the stack limit of
//    `count` and `countA`
// 2: Test the performance of `countC` and `countAC`


(* Exercise 8.5 (HR 11.1) *)

/// Generate an infinite sequence of odd numbers.
let allNum = Seq.initInfinite (fun i -> i)
let oddNums = Seq.filter (fun i -> i%2<>0) allNum
let _ = oddNums : seq<int>


(* Exercise 8.6 (HR 11.2) *)
let rec factA = function
    | (0, acc) -> acc
    | (n, acc) -> factA ((n-1), (n*acc))
/// Generate an infinite sequence of factorial numbers
let factNum = Seq.initInfinite (fun i -> factA (i,1))
let _ = factNum : seq<int>

