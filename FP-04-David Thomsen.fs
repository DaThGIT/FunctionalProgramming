(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment four                    *)

module a4

(* Exercise 4.1 *)

/// Non-recursive
let explode (x:string) = List.ofArray (x.ToCharArray())
let _ = explode : string -> char list

/// Recursive
let rec explode2 = function
    | "" -> []
    | x -> x.[0]::explode2 (x.Remove(0,1))
let _ = explode2 : string -> char list

(* Exercise 4.2 *)

// Notes:       
// in the cases, where List.fold and List.Foldback is used, consider,
// that it is only a matter of a switch on the direction of the recursive call on the list
// and the function. It is not clear whether it is a matter of order in the string(example below), 
// or traversal of the list - ['a';'b';'c'] becomes "cba" since foldback first writes c 
// from the list to the accumulator. This could be the conclusion to why i cannot manage a recursive implementation
// of reversed implosion.
(*example: *)
/// implosion
// fold: 
// y acc -> y + acc
// y =  ['a'; 'b'; 'c'] & acc = ""
// ['a'; 'b'; 'c'] "" -> ['b'; 'c'] "a" -> ['c'] "ab" -> [] "abc"  

/// reversed implosion
// fold:
// y acc -> acc + y
// y =  ['a'; 'b'; 'c'] & acc = ""
// ['a'; 'b'; 'c'] "" -> ['b'; 'c'] "a" -> ['c'] "ba" -> [] "cba"

(* Regular implosion *)
let rec implode = function
    | [] -> ""
    | (y:char)::xs -> string y + (implode xs)
let _ = implode : char list -> string 

(* alternative implementations *)

/// List.foldBack
let helperfuncImp1 = (fun (s:char) acc -> string s + acc) 
let implode1 (s: char list) = List.foldBack helperfuncImp1 s "" 
let _ = implode1 : char list -> string

/// List.fold
let helperfuncImpFold = (fun y acc -> y + string acc)
let implodeFold (f: char list) = List.fold helperfuncImpFold "" f
let _ = implodeFold : char list -> string 


(* Reversed implosion *)
let helperfuncImpRev = (fun (y:char) acc -> acc + string y)
let implodeRev j = List.foldBack helperfuncImpRev j "" 
let _ = implodeRev : char list -> string 

(* alternative implementations *)

/// recursive (with some helper methods)
// just gives regular implosion results: ['a'; 'b'; 'c'] = "abc" <- solution could be go backwards through the list
let listRev ls = List.rev ls
let _ = listRev : 'a list -> 'a list 
let rec listToString = function
    | []           -> ""
    | (y:char)::xs -> string y + (implode xs)  
let _ = listToString : char list -> string      
let implodeRev1 ls = listToString (listRev ls)
let _ = implodeRev1 : char list -> string 

/// List.fold
let helperfuncImpRevFold = (fun y acc -> string acc + y)
let implodeRevFold (j: char list) = List.fold helperfuncImpRevFold "" j 
let _ = implodeRevFold : char list -> string 

(* END OF Exercise 4.2 *)


(* Exercise 4.3 *)
/// Without function composition
let toUpperHelperfunc c = System.Char.ToUpper c
let toUpper s = implode (List.map toUpperHelperfunc (explode s))
let _ = toUpper : string -> string 

/// With forward function composition:
let toUpper1 = explode >> List.map System.Char.ToUpper >> implode 
let _ = toUpper1 : string -> string

//// With backward function composition and pipe-forward
let toUpper2 x = explode x |> (implode << List.map toUpperHelperfunc) 
let _ = toUpper2 : string -> string

(* Exercise 4.4 *)

/// Checks whether given string is a palindrome
let toUpperS x = implode (List.map toUpperHelperfunc (explode x)) // identical to the toUpper implementation above(4.3). 
let _ = toUpperS : string -> string

let explodeRev (x:string) = List.ofArray (Array.rev (x.ToCharArray()))
let _ = explodeRev : string -> char list 

let palindrome x = (toUpperS >> explode) x = (toUpperS >> explodeRev) x
let _ = palindrome : string -> bool 

(* Exercise 4.5 *)

/// The Ackerman function. Expects a non-negative input.
let rec ack (m,n) = 
    match (m,n) with
    | (0,n) -> n+1
    | (m,0) -> ack(m-1, 1)
    | (m,n) -> ack(m-1,ack(m, n-1))
let _ = ack : int * int -> int

// ack (3,11)
// val it : int = 16381

(* Exercise 4.6 *)

/// Helper function given in assignment description
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)
let _ = time : (unit -> 'a) -> 'a * System.TimeSpan

/// Time a function, which is given a single argument
let timeArg1 f a = time (fun () -> f a)
let _ = timeArg1 : ('a -> 'b) -> 'a -> 'b * System.TimeSpan
(*
> time (fun () -> ack(3,11));;
> val it : int * System.TimeSpan =
  (16381, 00:00:00.5266306 {Days = 0;
                            Hours = 0;
                            Milliseconds = 526;
                            Minutes = 0;
                            Seconds = 0;
                            Ticks = 5266306L;
                            TotalDays = 6.095261574e-06;
                            TotalHours = 0.0001462862778;
                            TotalMilliseconds = 526.6306;
                            TotalMinutes = 0.008777176667;
                            TotalSeconds = 0.5266306;})

*)

(* Exercise 4.7 *)

/// Run a function with two arguments, (1) a falling integer index (down to 1)
/// and (2) a generic argument, which also defines the return type.
let rec downto1 f n e = 
    match n with
    | n when n <= 0 -> e
    | n -> downto1 f (n-1) (f (n, e))
let _ = downto1 : (int * 'a -> 'a) -> int -> 'a -> 'a

/// Using downto1 calculates the factorial of the given argument.
let downtoFact n = downto1 (fun (a,b) -> a*b) n 1
let _ = downtoFact : int -> int
let applyGandCons g (n,e) = g(n)::e

/// Build a list containing elements with a function applied to the elements'
/// respective indices.
let intFuncToList g n = downto1 (fun (n,e) -> g(n)::e) n []
let _ = intFuncToList : (int -> 'a) -> int -> 'a list

(* END OF HANDIN *)


(* stuff from lecture 5 *)
type Tree = Leaf
          | Node of Tree * int *Tree 

let rec insert i = function
    | Leaf                    -> Node(Leaf, i, Leaf)
    | Node(t1, j, t2) as tr   -> 
        match compare i j with // compare returns 0 if i = j; -1 if i < j; 1 if i > j
        | 0             -> tr
        | n when n < 0  -> Node(insert i t1, j, t2)
        | _             -> Node(t1, j, insert i t2)
let _ = insert : int -> Tree -> Tree 

let rec memberOf i = function
    | Leaf              -> false
    | Node(t1, j, t2)   -> match compare i j with
                           | 0            -> true
                           | n when n < 0 -> memberOf i t1
                           | _            -> memberOf i t2
let _ = memberOf : int -> Tree -> bool 

let rec inOrder = function
    | Leaf              -> []
    | Node(t1, j, t2)   -> (inOrder t1) @ [j] @ (inOrder t2)
let _ = inOrder : Tree -> int list 

let rec preOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr)
let _ = preOrder : Tree -> int list

let rec postOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> (postOrder tl) @ (postOrder tr) @ [x]
let _ = postOrder : Tree -> int list 

let rec delMin = function
    | Node(Leaf, i ,t2) -> (i, t2)
    | Node(t1, i, t2)   -> let (m, t1') = delMin t1
                           (m, Node(t1', i, t2))
let _ = delMin : Tree -> int * Tree 

type Tree<'a> = Leaf | Node of Tree<'a> * 'a * Tree<'a>

let rec inFoldBack f t e = 
    match t with 
    | Leaf            -> e
    | Node(t1, x, t2) -> let er = inFoldBack f t2 e
                         inFoldBack f t1 (f x er)
let _ = inFoldBack : ('a -> 'b -> 'b) -> Tree<'a> -> 'b -> 'b
(*
let rec delete j = function // type mismatch exception so far
  | Leaf          -> Leaf
  | Node(t1,i,t2) ->
       match compare i j with
       | n when n<0 -> Node(t1,i,delete j t2)
       | n when n>0 -> Node(delete j t1,i,t2)
       | _          ->
            match t2 with
            | Leaf -> t1
            | _    -> let (m,t2') = delMin t2 (* <- expects int tree, gets tree *)
                      Node(t1,m,t2' (* <- expects int tree, gets tree *))
let _ = delete : 
*)

type Fexpr =
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr 

let rec D = function 
    | Const _       -> Const 0.0
    | X             -> Const 1.0
    | Add(fe1, fe2) -> Add(D fe1, D fe2)
    | Sub(fe1, fe2) -> Sub(D fe1, D fe2)
    | Mul(fe1, fe2) -> Add(Mul(D fe1, fe2), Mul(fe1, D fe2))
    | Div(fe1, fe2) -> Div(
                           Sub(Mul(D fe1, fe2), Mul (fe1, D fe2)),
                           Mul(fe2,fe2))
let _ = D : Fexpr -> Fexpr 

let rec compute x = function
    | Const r -> r
    | X       -> x
    | Add(fe1, fe2) -> compute x fe1 + compute x fe2
    | Sub(fe1, fe2) -> compute x fe1 - compute x fe2
    | Mul(fe1, fe2) -> compute x fe1 * compute x fe2
    | Div(fe1, fe2) -> compute x fe1 / compute x fe2
let _ = compute : float -> Fexpr -> float 

type FileSys = Element list
and Element = 
    | File of string
    | Dir of string * FileSys 

let d1 = 
    Dir("d1",[File "a1";
              Dir("d2", [File "a2";
                         Dir("d3", [File "a3"])]);
              File "a4";
              Dir("d3", [File "a5"])
             ])
let _ = d1 : Element 

let rec namesFileSys = function 
    | []    -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
  and namesElement = function 
    | File s     -> [s]
    | Dir(s, fs) -> s :: (namesFileSys fs)
let _ = namesFileSys : Element list -> string list 
let _ = namesElement : Element -> string list 

type ExprTree = 
    | Const of int
    | Ident of string 
    | Minus of ExprTree
    | Sum   of ExprTree * ExprTree
    | Diff  of ExprTree * ExprTree
    | Prod  of ExprTree * ExprTree
    | Let   of string * ExprTree * ExprTree 

let rec eval t env = 
    match t with
    | Const n      -> n
    | Ident s      -> Map.find s env
    | Minus t      -> - (eval t env)
    | Sum(t1, t2)  -> eval t1 env + eval t2 env
    | Diff(t1, t2) -> eval t1 env - eval t2 env
    | Prod(t1, t2) -> eval t1 env * eval t2 env
    | Let(s, t1, t2)  -> let v1   = eval t1 env
                         let env1 = Map.add s v1 env
                         eval t2 env1 
let _ = eval : ExprTree -> Map<string, int> -> int 