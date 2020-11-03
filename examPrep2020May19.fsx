// functional exam prep 2020

module maj2019a

(* Question 1 *)

// Question 1.1

let infSeq3 = Seq.initInfinite (fun i -> i * 3)
let _ = infSeq3 : seq<int>

let infSeqCached = Seq.cache infSeq3
let _ = infSeqCached : seq<int>

let finSeq3 n = Seq.init n (fun i -> Seq.item i infSeqCached)
let _ = finSeq3 : int -> seq<int>

let sumSeq3 n = Seq.sum (finSeq3 n) 
let _ = sumSeq3 : int -> int

// Question 1.2 
let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }
let _ = seqMap2 : ('a->'b->'c)->seq<'a>->seq<'b>->seq<'c>

let testF = (fun x y -> x+y)
let s1 = List.toSeq [1; 3; 3]
let s2 = List.toSeq [4; 5; 2] 

// Insert description here:
// The method seqMap2 takes two sequences and a function. It converts each corresponding index of the two sequences
// to a tuple (pair), and applies the function on them. 
// For instance: >seqMap2 testF s1 s2;; returns a new sequence (seq[5;8;5]), where the corresponding indicies 
// have been added together, as defined by the function. 

let swap (x,y) = (y,x)
let _ = swap : 'a * 'b -> 'b * 'a
// The swap function does not hold the correct number of functions expected. It holds the one funcion a -> b, 
// but not the second one b -> c. Eventhough it has both an x and y variable, these are considered as a single
// entity by the system, since they are a tuple (pair). 

let fix s = (fun x y -> s (x,y))
let _ = fix : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(*
> seqMap2 (fix swap) s1 s2;;
val it : seq<int * int> = seq [(4, 1); (5, 3); (2, 3)]
*)

(* Question 2 *)

// Question 2.1

type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a', false, [TN('n', true ,[TN('d', true, [])])])

let trie03 = TN('a', false, [TN('n', true, [TN('d', true, [])]);
                             TN('d', false,[TN('d', true, [])]);
                             TN('t', true, [])])

let trie04 = TN('a', false, [TN('n', true, [TN('d', true, [])]);
                             TN('d', false,[TN('d', true, [])]);
                             TN('t', true, [TN('x', false, [])])])

exception TrieError of string

// Question 2.2
// check higherOrder functions (List.fold/length/map/foldback)
// evt kig på at implementere fold for TrieNode. 

let rec numLetters = function
    | TN(_,_,[])    -> 1
    | TN(a,b,list)  -> 1 + List.fold (fun acc tr -> acc + numLetters tr) 0 list
let _ = numLetters : TrieNode<'a> -> int when 'a : equality
(*
> numLetters trie04;;
val it : int = 7
*)

let rec numWords = function
    | TN(a,true,list)  -> 1 + List.fold (fun acc tr -> acc + numWords tr) 0 list
    | TN(a,false,list) -> List.fold (fun acc tr -> acc + numWords tr) 0 list
let _ = numWords : TrieNode<'a> -> int when 'a : equality

(*
> numWords trie04;;
val it : int = 4
*)

let listToString list = List.fold (fun acc x -> acc + (x:string)) "" list 

 


(* Question 3 *)

// Question 3.1

let F m i n k = 
    let rec inner m i n k = 
        match k with 
        | k when k <= 0 -> m
        | k when k > 0  -> inner m i n (k - 1) * (1.0 + i / n) // <- not tailrecursive 
        | _             -> failwith "not one of the cases defined by the assignment"
    inner m i n k 
let _ = F : float -> float -> float -> int -> float

// It is not tailrecursive, since the computational step of multiplications happens after the recursive call 

let FA m i n k = 
    let rec inner (acc:float) (k:int) = 
        match k with 
        | k when k <= 0 -> acc
        | k when k > 0  -> inner (acc * (1.0 + i / n)) (k-1) // here we have, that the recursive call is the last, thing that happens, and the multiplication computation is scheduled within the call to  'inner'
        | _             -> failwith "not one of the cases defined by the assignment"
    inner m k
let _ = FA : float -> float -> float -> int -> float

(*
> FA 100.0 0.1 1.0 0;;
val it : float = 100.0

> FA 100.0 0.1 1.0 10;;
val it : float = 259.374246
*)

// Question 3.2

let tabulate f start step stop = function
    | (x,y)
    

let _ = tabulate : (int -> 'a) -> int -> int -> int -> (int * 'a) list 

(* Question 4 *)