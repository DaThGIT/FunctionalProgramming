(* David Thomsen - davt@itu.dk        *)
(* Functional Programming spring 2020 *)
(* Assignment seven                   *)

module a7

(* Exercise 7.1 (HR 9.1) *)

(* Reproduce the stack after a call, `g 2`, where `g` is the following:
 *
 * let xs = [1;2]
 * let rec g = function
 *     | 0 -> xs
 *     | n -> let ys = n::g(n-1)
 *            List.rev ys
 *
 * Write a visualisaion of the stack and heap below:
 *
 *)

 (* 
    Visualisation of push       
        Stack                          Heap

        n[0]
SF3     result          -------------> xs
        _________
        n[1]
        ys              -------------> [1|r]                            r = result of n-1
SF2     result[?]
        _________
        n[2]
        ys              -------------> [2|r]                            r = result of n-1
SF1     result[?]        
        _________
        g               -------------> 'closure'
        xs              -------------> [1| ] -> [2|x]
SF0     result[?]

    Visualisation of pop
        Stack                          Heap

        n[0]
SF3     result          -------------> xs
        _________
        n[1]
        ys              -------------> [1| ] -> xs
SF2     result          -------------> [2| ] -> [1| ] -> [1|x] 
        _________
        n[2]
        ys              -------------> [2| ] -> [2| ] -> [1| ] -> [1|x]
SF1     final result    -------------> [1| ] -> [1| ] -> [2| ] -> [2|x]
        _________
        g               -------------> 
SF0     xs              -------------> [1| ] -> [2|x]
 *)


(* Exercise 7.2 (HR 9.3) *)
let rec sumNaive = function 
    |(m, 0) -> m + 0
    |(m, n) -> m + n + sumNaive(m, n-1)
let _ = sumNaive : int * int -> int

let rec f = function
    | (m,n,acc) when m < 0 || n < 0 -> -1   

    | (m,0,acc)                     -> m + acc + 0 
    | (m,n,acc)                     -> f(m, n-1, m + n + acc)
let sum(m,n) = f(m,n,0)
let _ = sum : int * int -> int


(* Exercise 7.3 (HR 9.4) *)
/// Compute the length of a list
let rec computeLength = function
    | ([], acc)   -> acc
    | (_::xs,acc) -> computeLength(xs, acc + 1)   
let _ = computeLength : 'a list * int -> int
let length xs = computeLength (xs,0)
let _ = length : 'a list -> int


(* Exercise 7.4 (HR 9.6) *)
/// Compute the n'th factorial number using continuations
let rec fC n c = 
    match n with
    | 0 | 1 -> c(1)
    | n     -> fC(n-1) (fun r -> c(n*r))    
let _ = fC : int -> (int -> 'a) -> 'a
let factC n = fC n id  
let _ = factC : int -> int

/// for comparison purposes:
let rec fA = function
    | (0, acc) -> acc
    | (n, acc) -> fA ((n-1), (n*acc))
let _ = fA : int * int -> int
let factA n = fA (n,1)
let _ = factA : int -> int

/// evaluation: 
(*

for i in xs12 do factA i;;
Real: 00:00:00.046, CPU: 00:00:00.046, GC gen0: 0, gen1: 0, gen2: 0

for i in xs12 do factC i;;
Real: 00:00:00.293, CPU: 00:00:00.281, GC gen0: 58, gen1: 1, gen2: 0

after testing the running time on the list 'xs12', by taking the factorial of every element of that list 
(see helper methods below), my results show that the fastest implementation of fact (factA) is the tail recursive one, 
that does not use CPS. 

*)

(* Exercise 7.5 (HR 8.6) *)

/// Compute the `nth` number of the Fibonacci sequence using a while loop.

let whFib n = 
    let mutable a = 0 
    let mutable b = 0
    let mutable c = 0
    let mutable n' = 0
    if n < 0 then c <- 1
    else
        while(n' < n) do  
            if (n'= 0) then c <- 1
            else c <- a+b
            b <- a
            a <- c
            n' <- n' + 1      
    c    
let _ = whFib : int -> int

(* Exercise 7.6.1 (HR 9.7.1) *)
/// fibonacci sequence, implemented using two accumulating parameters 'n1 + n2'.
let rec fbA n n1 n2 =
    match n with
    | n when n = 0  -> 0  
    | n when n = 1  -> n1+n2
    | n when n >= 2 -> fbA(n-1) (n1+n2) n1  
    | _ -> failwith "unpredicted corner case"
let _ = fbA : int -> int -> int -> int
let fibA n = fbA n 0 1
let _ = fibA : int -> int

(* Exercise 7.6.2 (HR 9.7.2) *)
let rec fbC n c =
    match n with
    | n when n = 0 -> c(0)
    | n when n = 1 -> c(1)
    | n when n > 1 -> fbC (n-2) (fun x -> fbC (n-1)(fun y -> c(x+y)))  
    | _            -> failwith "unpredicted cornercase"    
let _ = fbC : int -> (int -> int) -> int
let fibC n = fbC n id
let _ = fibC : int -> int 

 // computes correctly, but stops responding/becomes very slow around the 40th fibonacci number
(* > fibC 40;;
   Real: 00:00:21.165, CPU: 00:00:21.125, GC gen0: 1687, gen1: 3, gen2: 0
   val it : int = 102334155

   > fibA 40;;
   Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
   val it : int = 102334155                                                 *)


(* HELPERS *)

// fact 12 is the last factorial sequence index that will fit in a 32-bit integer
let xs12 = List.init 1000000 (fun i -> 12)

let xs45 = List.init 1000000 (fun i -> 45)

//#time
for i in xs45 do let _ = whFib i in ()
for i in xs45 do let _ = fibA i in ()
for i in xs45 do let _ = fibC i in () // CAREFUL: interactive will not terminate
(* END OF HELPERS *)

(*

comparison:

for i in xs45 do let _ = whFib i in ();;
Real: 00:00:00.155, CPU: 00:00:00.156, GC gen0: 0, gen1: 0, gen2: 0

for i in xs45 do let _ = fibA i in ();;
Real: 00:00:00.207, CPU: 00:00:00.203, GC gen0: 0, gen1: 0, gen2: 0

for i in xs45 do let _ = fibC i in ()
BREAKS interactive (does not terminate).

*)

(*END OF HANDIN*)