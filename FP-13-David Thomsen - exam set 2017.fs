/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  13  #############################
/// ###########################################################################
/// Author: David Thomsen <davt@itu.dk> - 2017 exam 
module a12

open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Numerics

type PrioritySet<'a when 'a: equality> =
    | EmptyPS
    | PrioritySet of List<'a>

let psEx = PrioritySet ["a";"b";"c"]
(*Question 1.1*)
// value of a priority stack in accordance with values given and inserted in problem formulation
let priSetEx = PrioritySet ["a";"q";"b";"d"]
// eval of priSetEx: val priSetEx : PrioritySet<string> = PrioritySet ["a"; "q"; "b"; "d"]
// The type being ProritySet of string
let empty = EmptyPS

(*Question 1.2*)

let isEmpty h = h = EmptyPS
//  > isEmpty psEx;;
//  val it : bool = false
//  > isEmpty empty;;
//  val it : bool = true

let rec size h =
    match h with 
    | EmptyPS -> 0
    | PrioritySet [] -> 0
    | PrioritySet (_::t) -> 1 + size (PrioritySet t)
//  > size psEx;;
//  val it : int = 3
//  > size priSetEx;;
//  val it : int = 4
let size1 l = // alternative implemenation using List.length
    match l with
    | EmptyPS -> 0
    | PrioritySet aList -> List.length aList

let contains e ps =
    match ps with 
    | EmptyPS -> false 
    | PrioritySet aList -> List.contains e aList
let _ = contains : 'a -> PrioritySet<'a> -> bool when 'a : equality 
//  > contains "b" psEx;;
//  val it : bool = true
//  > contains "a" priSetEx;;
//  val it : bool = true
//  > contains "e" priSetEx;;
//  val it : bool = false
//  > contains "b" empty;;
//  val it : bool = false
let getPN2 e ps = // if an element is in the PrioritySet, get the elements index number. Not working tried List.findindex. <- in all methods below, then main problem is that List.findindex forces e to be (a' -> bool) instead of a'
    if contains e ps = false 
    then failwith "PrioritySet does not contain queried element" 
    else List.findIndex 

let getPN1 e ps = // if an element is in the PrioritySet, get the elements index number. Not working tried pattern matching
    match ps with 
    | EmptyPS -> failwith "empty prioritySet, does not contain queried element"
    | PrioritySet aList -> List.findIndex e aList

let getPN3 e ps = if contains e ps = false then failwith "PrioritySet does not contain queried element" else List.findIndex// e ps// if an element is in the PrioritySet, get the elements index number. Not working, tried if then else

let getPN e ps = // if an element is in the PrioritySet, get the elements index number. Working implementation from Kalin. Notice no use of List.findindex.
  if (contains e ps) then  // see how cleanly he does the if check
        match ps with 
        | EmptyPS -> -1
        | PrioritySet aList ->  // like you considered, but <- "unpacking" the list in the PrioritySet here, solving the problem, that occurs in above methods concerning ('a -> bool) instead of 'a  
            let rec getRecidx list index =  // then nest the rec let declaration after you make it handle a list instead of a prioritySet.
                match list with 
                | [] -> index
                | x::_ when e = x -> index+1                                                    // does not give the correct priority number, rather it gives the index number. This should be plus index+1, i just can't make it "fit".  
                | _::t -> getRecidx t (index+1)     
            getRecidx aList -1                                                                                                              // if changed to +1 we will get index numbers like:
   else failwith "PrioritySet does not contain queried element"  // else condition in the end, notice proper indentation                    // > getPN "a" psEx;;                   
let _ = getPN : 'a -> PrioritySet<'a> -> int when 'a : equality                                                                             // val it : int = 2                           
//  > getPN "c" psEx;;                                                                                                                      // > getPN "c" psEx;;
//  val it : int = 2                                                                                                                        // val it : int = 4
//  > getPN "a" psEx;;                                                                                                                      // > getPN "d" priSetEx;;       
//  val it : int = 0                                                                                                                        // val it : int = 5                               
//  > getPN "d" priSetEx;;                                                                                                                  // either its an one higher that the expected priority number, or its one lower.         
//  val it : int = 3
//  > getPN "q" priSetEx;;
//  val it : int = 1

(*Question 1.3*)
// removes first element in a list, that is equal to e. 
let remove e ps =
  if (contains e ps) then  
    match ps with
    | EmptyPS -> EmptyPS
    | PrioritySet aList ->
          let rec rmvRec e list = // implementation from url: http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list. Using first occurrence, since no duplicates in set
              match list with                                                                                                  // function calls for alternative implementation: 
              | h::t when e = h -> t                                                                                           // let s = Seq.singleton e
              | h::t -> h::rmvRec e t                                                                                          // List.except s aList                    
              | _ -> []
          (PrioritySet(rmvRec e aList))
  else failwith "PrioritySet does not contain queried element"
//  > remove "b" psEx;;
//  val it : PrioritySet<string> = PrioritySet ["a"; "c"]
(*
let s = Seq.singleton e
List.except s aList
*)
let _ = remove : 'a -> PrioritySet<'a> -> PrioritySet<'a> when 'a : equality          
(* let remove e ps = // failed implementation using skeleton from getPN
    if (contains e ps) then
        match ps with
        | EmptyPS -> EmptyPS
        | PrioritySet aList -> 
                                let rec rmvIdx list idx =
                                    match list with 
                                    | [] -> []
                                    | x::xs when e = x -> *) 
// inserts the element with lowest priority (at tail),
let add e ps = 
   if not(contains e ps) then
     match ps with
     | EmptyPS -> EmptyPS
     | PrioritySet aList -> 
        let rec addRec e list =
            match list with 
            | [] -> [e]
            | x::xs -> x::(addRec e xs)
        (PrioritySet (addRec e aList))
   else ps
let _ = add : 'a -> PrioritySet<'a> -> PrioritySet<'a> when 'a : equality
//  > add "e" psEx;;
//  val it : PrioritySet<string> = PrioritySet ["a"; "b"; "c"; "e"]
//  > add "b" psEx;;
//  val it : PrioritySet<string> = PrioritySet ["a"; "b"; "c"]

(*Question 1.4*)
//  maps a function over a priorityset. 
let map f ps = 
    match ps with 
    | EmptyPS -> EmptyPS
    | PrioritySet aList ->
        let rec mapRec f list = 
            match list with 
            | [] -> []
            | xs -> List.map f xs 
        (PrioritySet(mapRec f aList))
let _ = map : ('a -> 'b) -> PrioritySet<'a> -> PrioritySet<'b> when 'a : equality and 'b : equality
//  > map id psEx;;
//  val it : PrioritySet<string> = PrioritySet ["a"; "b"; "c"]
//  > map (fun (c:string) -> c.ToUpper()) psEx;;
//  val it : PrioritySet<string> = PrioritySet ["A"; "B"; "C"]
//  for two priority sets returns a priority set, with the cartesian product of both input sets.  

(* IMPLEMENTATION NOT WORKING PROPERLY
let cp ps1 ps2 = // skeleton taken from stackOverflow url: https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
    match ps1, ps2 with
    | EmptyPS, EmptyPS -> EmptyPS
    | PrioritySet aList, PrioritySet bList ->
        let rec cartesianOfLists xs ys =
            match xs, ys with
            | _, [] -> []
            | [], _ -> []
            | x::xs', _ -> (List.map (fun y -> x, y) ys) @ (cartesianOfLists xs' ys) // function directly used in referenced url.
        PrioritySet cartesianOfLists <- this is where it goes wrong!
*)
let cp ps1 ps2 = // skeleton taken from stackOverflow url: https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
     if (isEmpty ps1 || isEmpty ps2) then EmptyPS
     else 
        match ps1, ps2 with 
        | EmptyPS, _ -> EmptyPS // redundant <- has error handling an empty PrioritySet
        | _, EmptyPS -> EmptyPS // redundant
        | PrioritySet set1, PrioritySet set2 ->
            let cartesianOfLists = set1 |> List.collect (fun x -> // se comment next line. 
                set2 |> List.map (fun y -> x, y)) // <- this function is essentially what i got from the stackoverflow url. 
            PrioritySet cartesianOfLists     
let _ = cp : PrioritySet<'a> -> PrioritySet<'b> -> PrioritySet<'a * 'b> when 'a : equality and 'b : equality
//  > cp psEx priSetEx;;
//  val it : PrioritySet<string * string> =
//    PrioritySet
//      [("a", "a"); ("a", "q"); ("a", "b"); ("a", "d"); ("b", "a"); ("b", "q");
//       ("b", "b"); ("b", "d"); ("c", "a"); ("c", "q"); ("c", "b"); ("c", "d")]
(*Question 2.1*)
let f curRow =
  let rec f' = function
       []   -> []
     | [_]  -> [1]
     | xs   -> let (x1::x2::xs) = xs
               x1 + x2 :: f' (x2::xs)
  (1 :: f' curRow)
let _ = f: int list -> int list 
  // the code above takes a list, adds the numbers from the list together, and returns a new list, with the numbers added and the old values.
  // for [] we get [1] as shown in second case of the function. for [1;1] we get [1;2;1], since 1 + 1 equal 2. Notice that the "sum" of the two elements, is added in the middle of the list.
  // for [1; 2; 1] we get [1; 3; 3; 1] since 1 + 2 = 3 and 2 + 1 = 3, agian notice, that it is in the middle indices, that the new integers are added. 
  // for [1; 3; 3; 1] we get [1; 4; 6; 4; 1] agian following the pattern of adding the head and head.next and tail and tail.last inwards.
  // the further we get in these computation, we notice that only the original head and tail values are kept, while we are increasing the values of the inner indices.
  // for [1; 4; 6; 4; 1] we get [1; 5; 10; 10; 5; 1], further demonstrating the above described computation pattern.
  (*Question 2.2*)
let fMatch curRow =
  let rec fMatch' curRow = 
    match curRow with
    | []  -> []
    | [_] -> [1]
    | x1::x2::xs -> x1 + x2::fMatch'(x2::xs) 
  (1 :: fMatch' curRow)
let _ = fMatch: int list -> int list
// So the problem that the first implementation, given in 2.1, has, is concerning the patternmatching. 
// Declaring the let (x1::x2::xs) = xs x1 + x2 :: f' (x2::xs) might provide the correct computations, however, 
// it leaves the compiler with cases, that are not accounted for. This is not the case, since the implementation does consider,
// the cases in the function, but the function is out of scope because of the indented let binding. 
// by doing it in pattern matching all cases are withing scope, and the compiler can see, that we do provide solutions to all cases. 
//  > fMatch [1; 4; 6; 4; 1];;
//  val it : int list = [1; 5; 10; 10; 5; 1]
(*Question 2.3*)
let fA curRow =
    let rec fA' aList acc = 
         match aList with
         | []  -> acc
         | [_] -> fA' [] (1::acc)
         | x1::x2::xs -> 
            let result = x1 + x2
            fA' (x2::xs) (result::acc) //<- makes it tailrecursive
    fA' curRow [1]            // this call does not ruin its tailrecursiveness, as thought earlier. 
let _ = fA: int list -> int list
//  > fA [1; 4; 6; 4; 1];; 
//  val it : int list = [1; 5; 10; 10; 5; 1]
(*Question 3.1*)
let seq1 = seq {  yield "hello"; yield "world"; yield "and"; yield "hello"; yield "world"; yield "again" }  // from url: https://www.bencode.net/f%23/2012/04/23/fsharpcheatsheet/?fbclid=IwAR0Es0Rwr_dxZNboudfGozzBOFnp9OYyiTtBEldy79EQlUHi6PQ4SfY52AE
let seq2 = seq { 1 .. 10 }
let seq3 = Seq.init 10 (fun n -> n * 2)
let seq4 = seq ['A';'B']
let seq5 = seq ['D';'E';'F']
let mySeq s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
              yield! [e1;e2] }
 // for every index in s2 the first index of s1 is put in front,
 // then the next index of s1 is put in front of every index of s2 and so on:
 // > mySeq seq2 seq3;;
 // val it : seq<int> = seq [1; 0; 1; 2; ...]
 //   > Seq.toArray it;;
 // val it : int [] =
 //      [|1; 0; 1; 2; 1; 4; 1; 6; 1; 8; 1; 10; 1; 12; 1; 14; 1; 16; 1; 18; 2; 0; 2;
 //        2; 2; 4; 2; 6; 2; 8; 2; 10; 2; 12; 2; 14; 2; 16; 2; 18; 3; 0; 3; 2; 3; 4;
 //        3; 6; 3; 8; 3; 10; 3; 12; 3; 14; 3; 16; 3; 18; 4; 0; 4; 2; 4; 4; 4; 6; 4;
 //        8; 4; 10; 4; 12; 4; 14; 4; 16; 4; 18; 5; 0; 5; 2; 5; 4; 5; 6; 5; 8; 5; 10;
 //        5; 12; 5; 14; 5; 16; 5; 18; ...|]

 // consider seq4 and seq5: calling 'mySeq seq4 seq5' provides the value asked for in the assignment.
 //  > mySeq seq4 seq5;;
 //  val it : seq<char> = seq ['A'; 'D'; 'A'; 'E'; ...]
 //  > Seq.toArray it;;
 //  val it : char [] =
 //  [|'A'; 'D'; 'A'; 'E'; 'A'; 'F'; 'B'; 'D'; 'B'; 'E'; 'B'; 'F'|]
(*Question 3.2*)
let (|EmptySeq|_|) a = if Seq.isEmpty a then Some () else None // to define an emptySeq, used in the patternmatching below <- got it from url: https://stackoverflow.com/questions/38907212/match-with-empty-sequence
let mySeq2 (s1: seq<'a>) s2: seq<'b> = 
    match s1, s2 with
        | EmptySeq, _ -> Seq.empty
        | _, EmptySeq -> Seq.empty
        | s1, s2 ->
            let cartesianOfLists = s1 |> Seq.collect (fun x -> // see comment next line. 
                s2 |> Seq.map (fun y -> x, y)) // <- this function is essentially what i got from the stackoverflow url. 
            cartesianOfLists     
let _ = mySeq2: seq<'a> -> seq<'b> -> seq<'a * 'b>
// > mySeq2 [1;2] ['A';'B';'C'];;
// val it : seq<int * char> = seq [(1, 'A'); (1, 'B'); (1, 'C'); (2, 'A'); ...]
// > Seq.toArray it;;
// val it : (int * char) [] =
// [|(1, 'A'); (1, 'B'); (1, 'C'); (2, 'A'); (2, 'B'); (2, 'C')|]
let mySeq3 n = Seq.initInfinite (fun i -> n*n - n*i) 
//  > val mySeq3 : n:int -> seq<int>
//  > mySeq3 10;;
//  val it : seq<int> = seq [100; 90; 80; 70; ...]