(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment two                     *)
module a2

// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = 
    let t2 = h1 * 60 + m1
    let t1 = h0 * 60 + m0
    t2 - t1
let _ = timediff : int * int -> int * int -> int 

// 2.2 function minutes
let minutes (h, m) =
    let midnight = (00,00)
    timediff midnight (h, m);;
let _ = minutes : int * int -> int

// 2.3 / HR 2.2
let rec pow (s, n) = String.replicate n s;;
let _ = pow : string * int -> string

let rec powPm (s, n) = 
    match n with 
    | n when n <= 0 -> ""
    | n -> s + powPm(s, n-1) 
let _ = powPm : string * int -> string

(* powPm ("hej", 3);;            *)
(* Evaluation:                   *)
(* val it : string = "hejhejhej" *)
// Above very similar to dupn from exercise 1: 
// The big difference being whether n is passed within the tuple (pow/powPm) or as an isolated variable (dupn)
let rec dupn a b =
    match b with
    | n when n <= 0 -> ""
    | n -> a + dupn a (n - 1)
let _ = dupn : string -> int -> string
(* Evaluation:                   *)
(* > dupn "hej" 3;;              *)
(* val it : string = "hejhejhej" *)

// 2.4 / HR 2.8
let rec bin = function
    | (a, 0) -> 1
    | (a, b) when a = b -> 1 
    | (a, b) -> bin(a-1,b-1) + bin(a-1,b)
let _ = bin : int * int -> int 

// 2.5 / HR 2.9
let rec f = function
| (0,y) -> y
| (x,y) -> f(x-1, x*y)
let _ = f : int * int -> int 
(* 
1. 
    type: int * int -> int    
    
2.
    the evaluation of f terminates whenever the x variable is decremented to the value of 0 
    and the subsequent return is the value of y.  

3. 
    evaluation: f(2,3) : val it : int = 6
    recursion formula: f(x-1, x*y)
    recursive trace: 
         f(2,3)
      -> f(2-1, 2*3)
      -> (1, 6)
      -> f(1-1, 1*6)
      -> (0, 6)
      -> 6
4. 
    The mathematical meaning of f(x,y) is (x! * y) - I do not quite understand this question in the book.   
    f(x, y) is a recursive 
*)

// 2.6 / HR 2.10
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)
let _ = fact : int -> int 
let test(c,e) = if c then e else 0;;
let _ = test : bool * int -> int
let testPolyT(c, e) = if false then fact -1 else 0 // maybe I misunderstand what the book wishes me to do?
let _ = testPolyT : 'a * 'b -> int  
(*
1.
    type: bool * int -> int
2.
    stackoverflow (because of factorial - 1)
3.
    evaluation:
              val testPolyT : 'a * 'b -> int
    
    by declaring false and fact -1 in the if statement, 
    we should be able to escape the stackoverflow caused by fact -1.
    I do though get stackoverflow from both implementations above. 

    fact(-1)
    -1 * fact(-1 - 1)
    -1 * -2
    -1 * (-1 * fact(-2 - 1)
    -1 * (-1 * -3)
    -1 * (-1 * (-1 * fact(-3 - 1)))
    -1 * (-1 * (-1 * -4))
    
    and so the recursive calculation will go on, be written to the stack, and in the end cause a stack overflow
    I dont see the difference in the two provided implementations, my results suggest the same.  
    If the intention is to provide a guard against negative input, this could be a solution:
*)

let rec factG = function    // > factG -1;;
    | n when n <= 0 -> 1    // val it : int = 1
    | n -> n * fact(n-1)    
let _ = factG : int -> int 

// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = 
    let g = 
        let h = 
            f(x,y)
        h
    g
let _ = curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

let uncurry g (x,y) = 
    let f =
        let h = 
            g x y
        h
    f
let _ = uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c