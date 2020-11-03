(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment 1                       *)
// Dear TA correcting this. I apologize for elements not required by the assignment, these are for my own learning purposes, and you are welcome to ignore them. 

module a1
// excercise 1.1
let sqr = fun x -> x*x
let _ = sqr : int -> int 

// excercise 1.2
let pow x n = System.Math.Pow(x, n)
let _ = pow : float -> float -> float
let powf = fun x n -> System.Math.Pow(x, n)
let _ = powf : float -> float -> float 
// recursive definition
let rec power = function
    | (_, 0) -> 1.0
    | (x, n) -> x * power(x, n-1)
let _ = power : float * int -> float
(* evaluation: f(2.0, 4): val it : float = 16.0     *)
(* recursion formula: x * power(x, n-1)             *)
(* recursive trace:                                 *)
(*    power (2.0, 4)                                *)
(* -> 2.0 * power(2.0, 4-1)                         *)
(* -> 2.0 * power(2.0, 3)                           *)
(* -> 2.0 * (2.0 * power(2.0, 3-1))                 *)
(* -> 2.0 * (2.0 * power(2.0, 2))                   *)
(* -> 2.0 * (2.0 * (2.0 * power(2.0, 2-1)))         *)
(* -> 2.0 * (2.0 * (2.0 * power(2.0, 1)))           *)
(* -> 2.0 * (2.0 * (2.0 * (2.0 * power(2.0, 1-1)))) *)
(* -> 2.0 * (2.0 * (2.0 * (2.0 * power(2.0, 0))))   *)
(* -> 2.0 * (2.0 * (2.0 * (2.0 * 1.0)))             *)
(* -> 16                                            *)

// excercise 1.3
let g n = n + 4
let _ = g : int -> int

// excercise 1.4
let h (x, y) = System.Math.Sqrt(x*x + y*y)
let _ = h : float * float -> float 
let hf = fun (x, y) -> System.Math.Sqrt(x*x + y*y)
let _ = hf : float * float -> float 

// excercise 1.5
let rec f = function
    |0 -> 0
    |n -> n + f(n-1)
let _ = f : int -> int
(* evaluation: f(4): val it : int = 10 *)
(* recursion formula: n + f(n-1)       *)
(* recursive trace:                    *)
(*    f 4                              *)
(* -> 4 + f(4-1)                       *)
(* -> 4 + f 3                          *)
(* -> 4 + (3 + f(3 - 1))               *)
(* -> 4 + (3 + f 2)                    *)
(* -> 4 + (3 + (2 + f(2 - 1)))         *)
(* -> 4 + (3 + (2 + f 1))              *)
(* -> 4 + (3 + (2 + (1 + f(1 - 1))))   *)
(* -> 4 + (3 + (2 + (1 + f 0)))        *)
(* -> 4 + (3 + (2 + (1 + 0)))          *)
(* -> 4 + (3 + (2 + 1))                *)
(* -> 4 + (3 + 3)                      *)
(* -> 4 + 6                            *)
(* -> 10                               *)

// excercise 1.6
let rec fib n = 
    match n with
    | 0 -> 0
    | 1 -> 1  
    | n -> fib(n-1) + fib(n-2)
let _ = fib : int -> int 
(* evaluation: f(4): val it : int = 3 *)
let rec fibf = function
    | 0 -> 0 
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)
let _ = fibf : int -> int 

// excercise 1.7
let rec sum = function 
    |(m, 0) -> m + 0
    |(m, n) -> m + n + sum(m, n-1)
let _ = sum : int * int -> int
// sum all elements of a list. 
let rec suml = function
    | [] -> 0
    | x::xs -> x + suml xs
let _ = suml : int list -> int 

// excercise 1.8 
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)
let _ = fact : int -> int 
let (x, y) = (System.Math.PI, fact -1)
// type: (float * int) -> stackoverflow (because of factorial -1)
let e = fact(fact 4)
// type: int
let c = power(System.Math.PI, fact 2)
// type: float 
let d = (power, fact)
// type: (float * int -> float) * (int -> int)

// excercise 1.9
let a = 5
let i a = a + 1
let g' b = (i b) + a
(* env: *)
// val a -> 5 : int 
// val i(a) -> a + 1 : int -> int
// val g'(b) -> (b + 1) + 5 : int -> int
(* evaluation: i(a): val it : int = 6 *)
(* evaluation: g'(3): val it : int = 9 *)

// excercise 1.10
let dup a:string = a + a 
let _ = dup : string -> string 

// excercise 1.11
let rec dupn (a:string) b =
    match b with
    | n when n <= 0 -> ""
    | b -> a + dupn a (b - 1)
let _ = dupn : string -> int -> string 