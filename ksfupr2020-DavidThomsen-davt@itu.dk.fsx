(* Functional Programming spring 2020 *)
(*              Exam set              *)
(*     David Thomsen - davt@itu.dk    *)
// I hereby declare that I myself have created this exam hand–in in its entirety without help from anybody else
module examMay2020

(* Question 1 *)
/// helperDefinitions
type mymap<'a,'b> = MyMap of list<'a*'b>

let ex1 = MyMap [('A',65);('B',66);('C',67)]
let ex1' = MyMap [('C',67);('A',65);('B',66)]

// Question 1.1 

let dice1 = MyMap [(1,4);(2,2);(3,4);(5,2);(5,2);(6,2)]
let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// val dice1 : mymap<int,int>
// val ex1 : mymap<char,int>
// The type of 'dice1' is mymap of int * int and the type of 'ex1' mymap of char * int. 
// They gain these monomorphic types, since they are concrete "instantiations" 
// of the polymorphic data type mymap of 'a * 'b 
// where respectively int is the key for dice1 and char is the key for ex1. 
// In both examples the values are of type int

let emptyMap() = MyMap []
let _ = emptyMap : unit -> mymap<'a,'b>

let size m = 
     match m with 
     | MyMap ls -> ls.Length
let _ = size : mymap<'a,'b> -> int

size ex1;;          // val it : int = 3
size (emptyMap());; // val it : int = 0

// Question 1.2 
let isEmpty m = 
    match m with 
    | m when (size m) = 0 -> true 
    | _                   -> false 
let _ = isEmpty : mymap<'a,'b> -> bool

isEmpty ex1;;          // val it : bool = false
isEmpty (emptyMap());; // val it : bool = true

let tryFind k (MyMap(m)) = 
    let rec inner k xs =
         match xs with 
         | []        -> None 
         | x::xs     -> let (k',v) = x 
                        let rest = xs  
                        if k = k' then Some(k',v)
                        else inner k rest
    inner k m
let _ = tryFind : 'a -> mymap<'a,'b> -> ('a * 'b) option when 'a : equality

tryFind 'A' ex1;; // val it : (char * int) option = Some ('A', 65)
tryFind 'B' ex1;; // val it : (char * int) option = Some ('B', 66)
tryFind 'D' ex1;; // val it : (char * int) option = None

// The equality constraint means, that whatever function will be called has to be of the same type as MyMap ('a*'b).
// This is important, since it would not be meaning full using Map.tryFind, since that presumes the dataType ('a, 'b)
// If the compiler (type system) did not force this constraint, alot of errors and unexpexted behavior would happen at run time.

let remove' k m =               // this does not work, it is not removing the tuple from the map 
    let rec inner ls =     
        match tryFind k m with  // tried to use tryFind from above, but I cant get the MyMap vs list difference correct 
        | None               -> m
        | Some(k',v)         -> 
             match ls with 
             | []                       -> MyMap (ls) 
             | x::xs' when (k = k')     -> MyMap (x::xs')
             | x::xs' when not (k = k') -> inner xs'
             | _                        -> failwith "unexpected cornercase" 
    inner (MyMap(m)) 
let _ = remove' : 'a -> mymap<'a,'b> -> mymap<'a,'b> when 'a : equality

let remove k (MyMap(m)) = 
    let rec inner k xs =
        match xs with 
        | []        -> []
        | x::xs     -> let (k',v) = x 
                       let rest = xs  
                       if k = k' then (k', v)::rest 
                       else inner k xs 
    MyMap (inner k m) 
let _ = remove : 'a -> mymap<'a,'b> -> mymap<'a,'b> when 'a : equality

remove 'B' ex1;;

let add k v m = 
    match tryFind k m with 
    | None          -> MyMap (List.fold (fun acc k v -> (k,v)::acc) (MyMap(m)))
    | Some(k',v)    -> m
                       
let _ = add : 'a -> 'b -> mymap<'a,'b> -> mymap<'a,'b> when 'a : equality

// Question 1.3 
let map f (MyMap(m)) = 
    match m with 
    | []    -> MyMap ([])
    | xs    -> MyMap (List.map (fun y -> (f y)) xs)
let _ = map : ('a -> 'b -> 'c) -> mymap<'a,'b> -> mymap<'a,'c>
// in general, i had alot of problems in this question with the MyMap type vs the List type. 
(* Question 2 *)

// Question 2.1
let even n = 
    match n with
    | n when n % 2 = 0 -> true 
    | _                -> false  
let _ = even : int -> bool

even 42;; // val it : bool = true
even 1;;  // val it : bool = false

let collatz n =
    match n with
    | n when (even n) -> n/2
    | _               -> 3*n+1  
let _ = collatz : int -> int 

collatz 45;; // val it : int = 136

let collatz' n =
    match n with
    | n when n > 0 && (even n)    -> n/2
    | n when not (n % 2 = 0)      -> 3*n+1
    | n when n < 1                -> failwith "n is zero or less"
    | _                           -> failwith "unexpected cornercase"   
let _ = collatz' : int -> int 

collatz' 45;; // val it : int = 136
collatz' 0;;  // System.Exception: n is zero or less

// Question 2.2

let applyN' f n N = // does NOT work 
    let rec inner acc N =
         match N with 
         | 1            -> acc 
         | N when N > 1 -> inner (f (f n)::acc) (N-1)
         | _            -> failwith "negative input"
    inner [] N       
let _ = applyN' : ('a -> 'a) -> 'a -> int -> 'a list
// I couldnt make it quite work. Allthough I believe I am almost there. 
// The problem is, that I keep calling the functions on the same value of n,
// not the latter computed value. I stopped here due to time constraint and
// I realized that it could be easily and cleanly implemented using mySeq. 
// A solution to the question can be seen below in 2.3, where I use mySeq to compute the list. 

let rec applyUntilOne f n = // i realize this is a hack
    match n with 
    | 1 -> (f n)*2               
    | _ -> applyUntilOne f (n-1) 
let _ = applyUntilOne : (int -> int) -> int -> int
applyUntilOne collatz 42;; // val it : int = 8

// Question 2.3 
let rec mySeq f x =
    seq { yield x               // <- put x in a seq             
          yield! mySeq f (f x)} // <- apply f to x && and call mySeq with the result
let _ = mySeq : ('a -> 'a) -> 'a -> seq<'a>

mySeq collatz 42;; // val it : seq<int> = seq [42; 21; 64; 32; ...]

// mySeq takes a function (f), and an argument 'a (x), in the case of the requested example an int. 
// It then computes a sequence of f applied to x, and f applied to the result of (f x). 
// In the example, the double application of f, is the reason for the changing values. 

let g x = x + x
let _ = g : int -> int

mySeq g 1;; // val it : seq<int> = seq [1; 2; 4; 8; ...]

// solution to Question 2.2 (applyN)
let applyN f n N = Seq.toList (Seq.take (N+1) (mySeq f n))
let _ = applyN : ('a -> 'a) -> 'a -> int -> 'a list

applyN collatz 42 8;; // val it : int list = [42; 21; 64; 32; 16; 8; 4; 2; 1]

(* Question 3 *)




(* Question 4 *)
/// helperDefinitions 
let rec dup = function
    [] -> []
    | x::xs -> x::x::dup xs
let _ = dup : 'a list -> 'a list

dup ["A";"B";"C";"D"];;
dup [1;2;3;4;5];;

// Question 4.1 

// dup takes a list and returns a new list with each element replicated. 
// Such that: 'dup [1;2;3;4;5]' -> val it : int list = [1; 1; 2; 2; 3; 3; 4; 4; 5; 5]
// Or: 'dup ["A";"B";"C";"D"]' -> 'val it : string list = ["A"; "A"; "B"; "B"; "C"; "C"; "D"; "D"]

let dupA ls = 
    let rec inner acc ls =
        match ls with
        | []    -> List.rev acc         // base case return accumulating paramater. (using an acc, the order will be opposite, there fore list.rev)  
        | x::xs -> inner (x::x::acc) xs // tail recursive call
    inner [] ls 
let _ = dupA : 'a list -> 'a list

dupA ["A";"B";"C";"D"];; // val it : string list = ["A"; "A"; "B"; "B"; "C"; "C"; "D"; "D"]
dupA [1;2;3;4;5];;       // val it : int list = [1; 1; 2; 2; 3; 3; 4; 4; 5; 5]  

// Question 4.2 

let replicate2 i = 
    match i with 
    | i -> Seq.ofList (dupA [i])
let _ = replicate2 : 'a -> seq<'a>

replicate2 4;; // val it : seq<int> = [4; 4] 

let mySeqinf = Seq.initInfinite (fun i -> replicate2 i)
let dupSeq = seq {for i in mySeqinf do yield! i}
let _ = dupSeq : seq<int>

dupSeq;; // val it : seq<int> = seq [0; 0; 1; 1; ...] 

// Question 4.3 

let dupSeq2 s =  seq {for i in s do yield! replicate2 i}
let _ = dupSeq2 : seq<'a> -> seq<'a>
dupSeq2 (seq[1;2]);; // val it : seq<int> = seq [1; 1; 2; 2]

(* END OF EXAM ASSIGNMENT *)