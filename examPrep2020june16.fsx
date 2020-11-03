(* Functional programming 2020 *)
(* Exam prep                 *)
(* June 2016               *)
(* David Thomsen         *)
(* davt@itu.dk         *)
module examPrep2016

(* Question 1 *)
/// helperDefinitions
type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])
let wrong = MSet (Map.ofList [("a",0);("b",2);("c",1)])

// Question 1.1 
let diceSet = MSet (Map.ofList [(1,2);(2,1);(3,5);(5,2);(6,2)]) // val diceSet : Multiset<int> =
                                                                //    MSet (map [(1, 2); (2, 1); (3, 5); (5, 2); (6, 2)])
// The type of diceSet is Multiset<int> 

// There is no problem holding a set of the functions mentioned. However, it makes no real sense using a multiSet,
// as keeping track of how many times we have each function, does not seem relevant.

// Question 1.2

let newMultiset() = MSet (Map.empty)
let _ = newMultiset : unit -> Multiset<'a> when 'a : comparison

let isEmpty = function
    | MSet map -> Map.isEmpty map 
let _ = isEmpty : Multiset<'a> -> bool when 'a : comparison    

isEmpty(newMultiset());; // val it : bool = true
isEmpty(ex)

let msTryFind (MSet(ms)) k = Map.tryFind k ms  
let _ = msTryFind : Multiset<'a> -> 'a -> int option when 'a : comparison

// Question 3.1 
let add k ms = 
   let (MSet(ms')) = ms   
   match msTryFind ms k with 
   | Some v -> MSet (Map.add k (v+1) ms')  
   | None   -> MSet (Map.add k 1 ms')  
let _ = add : 'a -> Multiset<'a> -> Multiset<'a> when 'a : comparison

add "a" ex;; // val it : Multiset<string> = MSet (map [("a", 2); ("b", 2); ("c", 1)])

let del k ms = 
   let (MSet(ms')) = ms   
   match msTryFind ms k with 
   | Some v when v > 1 -> MSet (Map.add k (v-1) ms')  
   | Some v when v = 1 -> MSet (Map.remove k ms')
   | None              -> MSet (ms')  
   | _                 -> failwith "unexpected cornercase" 
let _ = del : 'a -> Multiset<'a> -> Multiset<'a> when 'a : comparison

del "c" ex;; // val it : Multiset<string> = MSet (map [("a", 1); ("b", 2)])


// Question 1.4  
let rec helperFuncToList k = function
    | v when v = 1 -> k::[] 
    | v when v > 1 -> k::(helperFuncToList k (v-1))  
    | _            -> failwith "negative input" 

let toList ms = 
    match ms with 
    | MSet map when map.IsEmpty -> []
    | MSet map                  -> List.rev (Map.fold (fun acc k v -> (helperFuncToList k v)@acc) [] map)   
let _ = toList : Multiset<'a> -> 'a list when 'a : comparison

toList ex;; // val it : string list = ["a"; "b"; "b"; "c"]

let fromList xs =
    let rec inner acc = function  
        | []            -> acc 
        | x::xs'        -> inner (add x acc) xs'
    inner (newMultiset ()) xs
let _ = fromList : 'a list -> Multiset<'a> when 'a : comparison

fromList ["a";"a";"b"];; // val it : Multiset<string> = MSet (map [("a", 2); ("b", 1)]) 

(* Question 2 *)
/// helperDefinitions
let rec f n =
   if n < 10 then "f" + g (n+1) else "f"
and g n =
   if n < 10 then "g" + f (n+1) else "g"
let _ = f : int -> string 
let _ = g : int -> string 


// Question 2.1 
// if f is given an even integer < 10, it will result in a string starting and ending with f
// if f is given an uneven integer < 10, it will result in a string ending with g, but still starting with f.
// if given any integer > 10 it will result in just a single f.
// g works in a similar fashion as above. Therefore:
g 0;; // val it : string = "gfgfgfgfgfg" 
// will generate the requested string

// for any argument, either when it itself is >= 10 or is incremented to >= 10 the else statement will be reached, 
// and only a single f will be added, terminating the recursion, hence no argument can make f produce a infinite string.

// Question 2.2
let rec fA n acc =
    match n with 
    | n when n >= 10 -> (acc + "f")  
    | n when n < 10  -> gA (n+1) (acc + "f")
    | _              -> failwith "unexpected input"
and gA n acc = 
    match n with 
    | n when n >= 10 -> (acc + "g")
    | n when n < 10  -> fA (n+1) (acc + "g")
    | _              -> failwith "unexpected input"
let _ = fA : int -> string -> string 
let _ = gA : int -> string -> string 

fA 0 "";; // val it : string = "fgfgfgfgfgf"
gA 0 "";; // val it : string = "gfgfgfgfgfg"



(* Question 3 *)
/// helperDefinitions
let myFinSeq (n,m) = seq { for i in [0 .. n] do
                            yield! seq { for j in [0 .. m] do yield j }}
let _ = myFinSeq : int * int -> seq<int>

Seq.toList (myFinSeq (1, 10));; // val it : int list =
                                //        [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
Seq.toList (myFinSeq (2, 10));; // val it : int list =
                                //        [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 0; 1; 2;
                                //         3; 4; 5; 6; 7; 8; 9; 10]
// Question 3.1 

// myFinSeq takes a tuple of int, with a n value and a m value. 
// it will then return a sequence of 0 .. n and 0 .. m, n+1 times, as shown in the examples above.

Seq.toList (myFinSeq (2, 2));; // val it : int list = [0; 1; 2; 0; 1; 2; 0; 1; 2]
// the first part of the required sequence is achieved by by passing the values (1,2), however,
// we cannot for any argument achieve the last part of the sequence [...; 0; 1], since it is not possible to stop
// the third repition. Given the arguments (2,2), it will produce the sequence in the example above. 

// Question 3.2 
let myFinSeq2 (n,m) = seq { for i in [0 .. n] do
                            yield i, seq { for j in [0 .. m] do yield j }}
let _ = myFinSeq2 : int * int -> seq<int*seq<int>>

Seq.toList (myFinSeq2 (3,3));; // val it : (int * seq<int>) list =
                                        [(0, seq [0; 1; 2; 3]); (1, seq [0; 1; 2; 3]); (2, seq [0; 1; 2; 3]);
                                         (3, seq [0; 1; 2; 3])]




(* Question 4 *)
