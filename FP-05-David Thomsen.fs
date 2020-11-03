(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment five                    *)
module a5
(* -------------------------------------------------------------- *)
(* Helper functions and definitions                               *)
(* Assignment 5.1-5.3                                             *)
(* -------------------------------------------------------------- *)
/// The binary tree implementation.
type 'a BinTree =
      Leaf
    | Node of 'a * ('a BinTree) * ('a BinTree)

/// Counts leafs and nodes (as a tuple) in a given BinTree
let rec countNodes t =
    let (.+.) (l1,n1) (l2,n2) = (l1+l2,n1+n2 + 1) // NOTE +1 node count here
    match t with
    | Leaf           -> (1,0)
    | Node (_, l, r) -> (countNodes l) .+. (countNodes r)
let _ = countNodes : 'a BinTree -> int * int

/// Traverses a given BinTree in pre-order
let rec preOrder = function
    | Leaf           -> []
    | Node (x, l, r) -> x :: preOrder l @ preOrder r
let _ = preOrder : 'a BinTree -> 'a list

let floatBinTree = Node(43.0, Node(25.0, Node(56.0, Leaf, Leaf),
                                         Leaf),
                              Node(562.0, Leaf,
                                          Node(78.0, Leaf,Leaf)))
let _ = floatBinTree : float BinTree

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


(* Exercise 5.1 *)

/// Traverse the given tree in-order and collect nodes' elements

let rec inOrder = function
    | Leaf          -> []
    | Node(n, l, r) -> (inOrder l) @ [n] @ (inOrder r)
let _ = inOrder : 'a BinTree -> 'a list

let rec inOrderPM t =
    match t with 
    | Leaf -> []
    | Node(n, l, r) -> inOrder l @ n :: inOrder r 
let _ = inOrderPM : 'a BinTree -> 'a list 


(* Exercise 5.2 *)

/// Map a function, f, over a given BinTree, t, in order
let rec mapInOrder f = function
    | Leaf                   -> Leaf
    | Node(n, leftT, rightT) -> let leftT' = mapInOrder f leftT
                                let root = f n
                                let rightT' = mapInOrder f rightT
                                Node(root, leftT', rightT') 
let _ = mapInOrder : ('a -> 'b) -> 'a BinTree -> 'b BinTree


let rec mapInOrder2 f t = 
  match t with
  | Leaf -> Leaf
  | Node(n,leftT,rightT) -> let leftT' = mapInOrder f leftT
                            let root = f n
                            let rightT' = mapInOrder f rightT
                            Node(root, leftT', rightT')
let _ = mapInOrder2 : ('a -> 'b) -> 'a BinTree -> 'b BinTree

let rec mapPostOrder f t = 
    match t with 
    | Leaf -> Leaf 
    | Node(n, leftT, rightT) -> let leftT' = mapPostOrder f leftT
                                let rightT' = mapPostOrder f rightT
                                let root = f n 
                                Node(root, leftT', rightT')
let _ = mapPostOrder : ('a -> 'b) -> 'a BinTree -> 'b BinTree

// from the implementation of mapPostOrder, returning same floatBinTree as MapInOrder, the conclusion must be, 
// that unless the return is in another form than binTree, e.g a list, the order in which we traverse, does not change
// the structure of the Tree. 

(* Exercise 5.3 *)

/// Fold a function, f, over a given BinTree, t, in order
let foldInOrder f x t = List.fold (fun acc n -> f n acc) x (inOrder t)
let _ = foldInOrder : ('a -> 'b -> 'b) -> 'b -> 'a BinTree -> 'b

(* -------------------------------------------------------------- *)
(* Helper definitions and functions                               *)
(* Assignment 5.4-5.6                                             *)
(* -------------------------------------------------------------- *)

/// A type alias for a map that stores the state of an executing
/// programme('s scope).
type state = Map<string,int>

/// Update a variable's state
let update = Map.add
let _ = update : string -> int -> state -> state

/// Lookup a variable's state in the given state, and return an option-type
/// containing it if it exists, else None.
let lookup = Map.tryFind
let _ = lookup : string -> state -> int option

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


/// Abstract syntax for arithmetical expressions
type aExp = N of int
          | V of string
          | Add of aExp * aExp
          | Mul of aExp * aExp
          | Sub of aExp * aExp
          | Inc of string

/// Abstract syntax for boolean expressions
type bExp = TT
          | FF
          | Eq of aExp * aExp
          | Lt of aExp * aExp
          | Neg of bExp
          | Con of bExp * bExp

/// Abstract syntax for statements
type stm = Ass of string * aExp
         | Skip
         | Seq of stm * stm
         | ITE of bExp * stm * stm
         | While of bExp * stm
         | IT of bExp * stm
         | Rep of stm * bExp

/// Interpret an arithmetic expressions
let rec A a s =
    match a with
    | N n       -> n
    | V x       -> Map.find x s
    | Add(a1,a2)-> A a1 s + A a2 s
    | Mul(a1,a2)-> A a1 s * A a2 s
    | Sub(a1,a2)-> A a1 s - A a2 s
    | Inc(x)    -> A (Add(V x, N 1)) s
    | _         -> failwith "Not implemented"
let _ = A : aExp -> state -> int

/// Interpret a boolean expression
let rec B b s =
    match b with
    | TT            -> true
    | FF            -> false
    | Eq (a1, a2)   -> (A a1 s) = (A a2 s)
    | Lt (a1, a2)   -> (A a1 s) < (A a2 s)
    | Neg(a1)       -> not (B a1 s)
    | Con(a1, a2)   -> B a1 s && B a2 s
    | _             -> failwith "Not implemented"
let _ = B : bExp -> state -> bool

/// Interpret an Abstract Syntax Tree (AST)
let rec I stm s =
    match stm with
    | Ass (x,a)         -> update x (A a s) s
    | Skip              -> s
    | Seq (stm1,stm2)   -> let f  = I stm1 s
                           I stm2 f 
    | While (b, stm)    -> if (B b s) then I (While(b, stm)) (I stm s) else s
    | ITE (b,stm1,stm2) -> if (B b s) then I stm1 s else I stm2 s
    | IT(b,stm)         -> if B b s then I stm s else s
    | Rep(stm,b)        -> I (While(Neg(b),stm)) s
    | _                 -> failwith "Not implemented"
let _ = I : stm -> state -> state
 (* END OF HANDIN *)