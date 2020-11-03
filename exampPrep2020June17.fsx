(* Functional programming 2020 *)
(* Exam prep                 *)
(* June 2017               *)
(* David Thomsen         *)
(* davt@itu.dk         *)
module examPrep2017
 
(* Question 1 *)
/// HelperDefinitions

type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let psEx = PrioritySet ["a";"b";"c"]

// Question 1.1

let priSetEx = PrioritySet ["a";"q";"b";"d"] // val priSetEx : PrioritySet<string> = PrioritySet ["a"; "q"; "b"; "d"]
// the type of priSetEx is PrioritySet<string>

let empty = PrioritySet []

// Question 1.2

let isEmpty = function
    |PrioritySet [] -> true
    |_              -> false 
let _ = isEmpty : PrioritySet<'a> -> bool when 'a : equality

isEmpty(empty);; // val it : bool = true

let size = function 
    | PrioritySet []    -> 0 
    | PrioritySet xs    -> List.length xs  
let _ = size : PrioritySet<'a> -> int when 'a : equality

size psEx;; // val it : int = 3 

let contains e ps =
    match ps with 
    | PrioritySet [] -> false
    | PrioritySet xs -> List.exists (fun y -> y = e) xs 
let _ = contains : 'a -> PrioritySet<'a> -> bool when 'a : equality

contains "b" psEx;; // val it : bool = true

let getPN e ps = 
    if contains e ps then 
       match ps with
       | PrioritySet [] -> failwith "Empty PrioritySet" // will never be reached, since contains check above
       | PrioritySet xs ->
            let rec inner acc xs = 
                match xs with 
                | []     -> acc
                | x::xs' -> if e = x then acc else inner (acc + 1) xs'
            inner 1 xs
    else failwith "PrioritySet does not contain queried element"    
let _ = getPN : 'a -> PrioritySet<'a> -> int when 'a : equality

getPN "a" psEx;; // val it : int = 1
getPN "b" psEx;; // val it : int = 2
getPN "c" psEx;; // val it : int = 3
getPN "k" psEx;; // System.Exception: PrioritySet does not contain queried element

// Question 1.3 

let remove e ps =
    if contains e ps then
        match ps with 
        | PrioritySet [] -> ps 
        | PrioritySet xs -> PrioritySet(List.filter (fun y -> not (y = e)) xs) 
    else ps         
let _ = remove : 'a -> PrioritySet<'a> -> PrioritySet<'a> when 'a : equality

remove "b" psEx;; // val it : PrioritySet<string> = PrioritySet ["a"; "c"]


let add e ps = 
    if not (contains e ps) then
        match ps with 
        | PrioritySet [] -> PrioritySet(e::[])
        | PrioritySet xs -> PrioritySet(List.rev(e::(List.rev xs))) // double list rev is not too smart, but i couldnt make @ work
    else ps                                                         // and i avioded traversel of the list, and hence no recursion                    
let _ = add : 'a -> PrioritySet<'a> -> PrioritySet<'a> when 'a : equality 

add "h" psEx;;                // val it : PrioritySet<string> = PrioritySet ["a"; "b"; "c"; "h"]
add "h" (PrioritySet []);;    // val it : PrioritySet<string> = PrioritySet ["h"]
add "h" (PrioritySet ["q"]);; // val it : PrioritySet<string> = PrioritySet ["q"; "h"]

// Question 1.4 

let map f ps = 
    match ps with 
    | PrioritySet [] -> empty
    | PrioritySet xs -> PrioritySet (List.map f xs) 
let _ = map : ('a -> 'b) -> PrioritySet<'a> -> PrioritySet<'b> when 'a : equality and 'b : equality

map (fun (c:string) -> c.ToUpper()) psEx;; // val it : PrioritySet<string> = PrioritySet ["A"; "B"; "C"]

let cp ps1 ps2 = // make cartesian product of two lists -> see the function below 
    if (isEmpty ps1 || isEmpty ps2) then failwith "PrioritySet 1 or 2 is empty" 
    match ps1, ps2 with
    | PrioritySet xs, PrioritySet ys -> PrioritySet (List.collect (fun x ->             // <- collect from xs
                                                     List.map (fun y -> x,y) ys) xs)    // <- map to ys                                          
let _ = cp : PrioritySet<'a> -> PrioritySet<'b> -> PrioritySet<'a * 'b> when 'a : equality and 'b : equality

cp (PrioritySet["A";"B";"C"]) (PrioritySet["h";"i"]);; // val it : PrioritySet<string * string> =
                                                       //            PrioritySet
                                                       //               [("A", "h"); ("A", "i"); ("B", "h"); ("B", "i"); ("C", "h"); ("C", "i")]

(* Question 2 *)
/// HelperDefinitions 
let f curRow =
    let rec f' = function
         [] -> []
       | [_] -> [1]
       | xs -> let (x1::x2::xs) = xs // <- uncovered pattern matches on local pattern match declaration
               x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)
let _ = f : int list -> int list

// Question 2.1 

// f computes Pascal's triangle, by taking out, in sequence, the two elements, adding them together,
// then storing the new value. 

// Question 2.2
let fMatch curRow =
    let rec fMatch' = function
         []         -> []
       | [_]        -> [1]
       | x1::x2::xs -> x1 + x2 :: fMatch' (x2::xs) // <- Solution, no local pattern match declaration 
    (1 :: fMatch' curRow)
let _ = fMatch : int list -> int list

// Question 2.3

let fA curRow =
    let rec fA' ls acc =
       match ls with  
       | []         -> acc
       | [_]        -> fA' [] (1::acc)
       | x1::x2::xs -> let res = x1 + x2 
                       fA' (x2::xs) (res::acc) // <- tail recursive call 
    fA' curRow [1] 
let _ = fA : int list -> int list

fA [1];;          // val it : int list = [1; 1]
fA [1; 1];;       // val it : int list = [1; 2; 1]
fA [1; 2; 1];;    // val it : int list = [1; 3; 3; 1]
fA [1; 3; 3; 1];; // val it : int list = [1; 4; 6; 4; 1]

(* Question 3 *)
/// HelperDefinitions

let mySeq s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
               yield! [e1;e2] }
let _ = mySeq : seq<'a> -> seq<'a> -> seq<'a>

// Question 3.1

// mySeq computes a Sequence from two Sequences. The new sequence is the result of each element from the first sequence
// being stored before each of the elements of the second list. 
// For instance, the sequence '['A';'D'; 'A';'E';'A';'F'; 'B';'D'; 'B';'E'; 'B';'F']' can be obtained by passing the two
// sequences: '['A';'B']' and '['D';'E';'F']'.
Seq.toList (mySeq (seq ['A';'B']) (seq ['D';'E';'F']));; // val it : char list = 
                                                         //   ['A'; 'D'; 'A'; 'E'; 'A'; 'F'; 'B'; 'D'; 'B'; 'E'; 'B'; 'F']

// Question 3.2 

let mySeq2 s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
               yield! [(e1,e2)] }
let _ = mySeq2 : seq<'a> -> seq<'b> -> seq<'a * 'b>

Seq.toList (mySeq2 (seq [1;2]) (seq ['A';'B';'C']));; // val it : (int * char) list =
                                                      //     [(1, 'A'); (1, 'B'); (1, 'C'); (2, 'A'); (2, 'B'); (2, 'C')]

// Question 3.3

let mySeq3 n = Seq.initInfinite (fun i -> n*n - n * i)
let _ = mySeq3 : int -> seq<int>
mySeq3 2;; // val it : seq<int> = seq [4; 2; 0; -2; ...]

(* Question 4 *)