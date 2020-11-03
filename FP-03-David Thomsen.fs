(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment three                   *)
module a3

open System.Windows.Forms
open System.Runtime.InteropServices

(* Exercise 3.1 *)
// with if, else, then statement. 
let rec downTo n =
    if n = 0 then []
    else [n.. -1.. 1]
let _ =  downTo : int -> int list 

// with pattern matching. 
let rec downTo2 n =
    match n with
    | 0 -> []
    | n -> [n.. -1.. 1] 
let _ = downTo2 : int -> int list 

(* Exercise 3.2 *)
let rec removeOddIdx xs =
    match xs with 
    | [] -> []
    | [x] -> [x]
    | x::r::xs -> x::(removeOddIdx xs) // x if odd, since cons operation to rest of list, r if remove even idx, since we keep the variable assigned.    
let _ = removeOddIdx : 'a list -> 'a list     

let rec removeEvenIdx xs = 
    match xs with
    | x::y::xs -> y::(removeEvenIdx xs)
    | _ -> []
let _ = removeEvenIdx : 'a list -> 'a list 

let rec removeOddIdx1 xs = 
    match xs with 
    | x::r::xs -> x::(removeOddIdx xs)
    | _ -> [] // with this implementation a list with a single value is converted to an empty list, which is not the case in the above
let _ = removeOddIdx1 : 'a list -> 'a list
(* Exercise 3.3 *)
let rec combinePair xs = 
    match xs with 
    | []       -> []
    | [x]      -> [] 
    | x::y::xs -> (x,y)::(combinePair xs)
let _ = combinePair : int list -> (int * int) list

(* Exercise 3.4 / HR exercise 3.2 *)
type CurrencyTuple = (int * int * int)

let rec CurrencyTuple = function
    | (po, sh, pe) when abs pe >= 12 -> CurrencyTuple (po, sh + (pe/12), pe%12)
    | (po, sh, pe) when abs sh >= 20 -> CurrencyTuple (po + (sh/20), sh%20, pe)
    | (po, sh, pe) when sh < 0 && po > 0 -> CurrencyTuple (po-1, sh+20, pe)
    | (po, sh, pe) when pe < 0 && sh > 0 -> CurrencyTuple (po, sh-1, pe+12)
    | (po, sh, pe) -> (po, sh, pe)


(*    if pe > 12 then CurrencyTuple (po, sh + (pe/12), pe%12)
    else if sh > 20 then (po + (sh/20), sh%20, pe)
    else (po, sh, pe);; *)

//tuple addition
let (.+.) (po1,sh1,pe1) (po2,sh2,pe2) = CurrencyTuple (po1+po2, sh1+sh2, pe1+pe2)

//tuple subtraction
let (.-.) (po1,sh1,pe1) (po2,sh2,pe2) = CurrencyTuple (po1-po2, sh1-sh2, pe1-pe2)

type CurrencyRecord = {pounds: int; shillings: int; pence: int}

let triple c = (c.pounds, c.shillings, c.pence)

let currRec (po,sh,pe) = {pounds = po ; shillings = sh ; pence = pe}

//record addition
let (..+..) a b = currRec ((triple a) .+. (triple b))

//record subtraction
let (..-..) a b = currRec ((triple a) .-. (triple b))

(* Exercise 3.5 / HR exercise 3.3 *)
type Complex = (float * float)
let (|+|) ((a, b): Complex) ((c, d): Complex) = (a+c,b+d)
let (|*|) ((a, b): Complex) ((c, d): Complex) = ((a*c)-(b*d),(b*c)+(a*d))
let (|-|) ((a, b): Complex) ((c, d): Complex) = (a-c,b-d)
let q ((a, b): Complex) = ((a*a)+(b*b))
let (|/|) ((a, b): Complex) ((c, d): Complex) = (a, b) |*| (c/q (c, d), -d/q (c, d))

(* Exercise 3.6 / HR exercise 4.4 *)
// book implementation of suml (sum list)
let rec suml = function
    | [] -> 0
    | x::xs -> x + suml xs
let _ = suml : int list -> int 

let rec altsum = function
    | [] -> 0
    | x0::xs -> x0 - altsum xs // same implementation as suml just changing addition to subtraction on the list. 
let _ = altsum : int list -> int    
(*  evaluation: altsum (downTo 4) -> val it : int = 2                *)
(*  recursion formula: x0 - altSum xs                                *)
(*  recursive trace:                                                 *)
(*  altsum[4;3;2;1]                                                  *)
(*  -> 4::[3;2;1]                                                    *)
(*  -> 4 - altsum[3;2;1]                                             *)
(*  -> 4 - (3::[2;1])                                                *)
(*  -> 4 - (3 - altsum[2;1])                                         *)
(*  -> 4 - (3 - (2::[1]))                                            *)
(*  -> 4 - (3 - (2 - altsum[1]))                                     *)
(*  -> 4 - (3 - (2 - (1::[])))                                       *)
(*  -> 4 - (3 - (2 - (1 - 0)))                                       *)
(*  -> 2                                                             *)

(* END OF HANDIN *)
// from lecture week 3 + week 4 - please ignore dear TA. 
let rec gcd = function
    |(0,n) -> n
    |(m, n) -> gcd(n % m,m)
let _ = gcd : int * int -> int 

let rec unzip = function
    | [] -> ([],[])
    | (x,y)::rest -> let (xs,ys) = unzip rest
                     (x::xs, y::ys)
let _ = unzip : ('a * 'b) list -> 'a list * 'b list

let listA = [(1, "a");(2, "b");(3, "c");(4, "d")]
let listB = [(1, 2.3);(2, 3.4);(3, 4.5);(4, 5.6)]
let listC = [1; 2; 3; 4; 5; 6; 7; 8]
let listD = [1; 2; 3; 4; 5; 6; 7]

type shape =
     Circle of float
    |Square of float
    |Triangle of float*float*float 

let area = function 
    | Circle r          -> System.Math.PI * r * r
    | Square a          -> a * a
    | Triangle (a,b,c)  -> 
        let s = (a + b + c)/2.0
        sqrt(s*(s-a)*(s-b)*(s-c))
let _ = area : shape -> float 

type 'a option = None | Some of 'a

let rec findPosI p x = function 
    |y::_ when x=y -> Some p
    |_::ys         -> findPosI (p+1) x ys
    |[]            -> None
let _ = findPosI : int -> 'a -> 'a list -> int option when 'a : equality
let findPos x ys = findPosI 0 x ys
let _ = findPos : 'a -> 'a list -> int option when 'a : equality

let rec (@) xs ys = // implementation of append 
    match xs with 
    | [] -> ys
    | x::xs' -> x::(xs' @ ys)
let _ = (@) : 'a list -> 'a list -> 'a list

let rec naive_rev = function // reverses a list O(n^2) running time. .net implementation is O(n).
    | [] -> []
    | x::xs -> naive_rev xs @ [x]
let _ = naive_rev : 'a list -> 'a list

let rec isMember x = function
    | [] -> false
    | y::ys -> x=y || isMember x ys
let _ = isMember : 'a -> 'a list -> bool when 'a : equality 

let rec sumProd = function
    | [] -> (0, 1)
    | x::rest -> let (rSum, rProd) = sumProd rest
                 (x+rSum, x*rProd)
let _ = sumProd : int list -> int * int

let rec split = function // can it be generic by replacing the 1 in the second case? yes it could
    | [] -> ([], [])
    | [x] -> ([x],[])
    | x::y::xs -> let (xs1, xs2) = split xs
                  (x::xs1, y::xs2)
let _ = split : 'a list -> 'a list * 'a list
