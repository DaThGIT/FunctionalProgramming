/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  12  #############################
/// ###########################################################################
/// Author: David Thomsen <davt@itu.dk>
module a12


type Heap<'a when 'a: equality> =
  | EmptyHP
  | HP of 'a * Heap<'a> * Heap<'a>

(* Q1.1 *)
let ex3 = HP (1, HP (2, HP (3, EmptyHP, EmptyHP), HP (5, EmptyHP, EmptyHP)), HP (4, EmptyHP, EmptyHP))
let ex4 = HP (2, HP (3, HP (4, EmptyHP, EmptyHP), HP (7, EmptyHP, EmptyHP)), HP (9, EmptyHP, EmptyHP))
let ex5 = HP (1, HP (5, HP (2, EmptyHP, EmptyHP), HP (5, EmptyHP, EmptyHP)), HP (3, EmptyHP, EmptyHP))
// val ex3 : Heap<int> =
//  HP
//    (1,HP (2,HP (3,EmptyHP,EmptyHP),HP (5,EmptyHP,EmptyHP)),
//     HP (4,EmptyHP,EmptyHP))                    
//
//  The type of ex3 is monomorphic. The type of the Heap is polymorhpic. This is signified in F# by the type int, 
//  in the case of ex3, and by the type 'a in the Heap. A polymorhpic type can be understood as a generic placeholder
//  for another type. We declare a Heap<int>, when declaring ex3, because we pass a node value as an int, and declare,
//  that all types must be equal, hence every other node value will be an int or empty. 
 

let empty = EmptyHP // empty heap

exception HeapError of string
(* Q1.2 *)
let isEmpty h = h = EmptyHP // is a heap h empty?

let rec size h = // counts the amount of nodes
    match h with
    | EmptyHP -> 0 
    | HP(_, hp1, hp2) -> 1 + size hp1 + size hp2 // change 1 to 'a, and it can count the sum of all nodes. 

// implementation of find using helpermethod to let find work on a list and get the head <- did not pass some codejudge tests.
(*
let rec hp2List f = // traverses heap returns heap elements as a list
  match f with
  | EmptyHP -> raise (HeapError "Heap is empty") // using the error declared in Q 1.1 
  | HP (n,hpL,hpR) -> n :: hp2List hpL @ hp2List hpR // preOrder traversal

let find h = hp2List h |> List.head // since preOrder traversal, just get the head of the list = lowest element, since Heap property fulfilled 
*)
// alternative, clean implemenation of find. Realized that i did not need all the list handling, if i just returned
// the root value n of a node like this: 
let find h = 
    match h with 
    | EmptyHP -> raise (HeapError "Heap is empty") // if root node is empty, the heap is empty
    | HP (n, hp1, hp2) -> n // since the heap property, the value of n in the first node, is the value of the root,
                            // and therefore the minimum value in the heap.

(*
// for comparing values of nodes, used in chkHeapProperty:
let comparator hp1 hp2 = 
  let v = compare hp1 hp2
  v = -1 || v = 0    

 
let chkHeapProperty h = // Checks Heap to confirm, that property is intact: value of parent nodes < child nodes
  let findfixed h v = if isEmpty h then v else find h  // for handling heapError thrown in find implementation. 
  let rec chk h =
    match h with
    | EmptyHP -> true  // given that the Heap property of an empty Heap is true.                                      
    | HP(v, hpL, hpR) -> let v1 = findfixed hpL v 
                         let v2 = findfixed hpR v 
                         comparator v v1 && comparator v v2 // using comparator from above, so i can compare values in nodes.
                         && chk hpL && chk hpR              // recursively checks the tree. 
  chk h
  *)

  // implementation without comparator. Does it fix the errors that chHeapProperty has been having concerning EmptyHP's
let rec chkHeapProperty h = 
    match h with 
    | EmptyHP -> true
    | HP(v, hpL, hpR) ->
        match hpL, hpR with 
        | EmptyHP, EmptyHP -> true
        | HP(v1,_,_), HP(v2,_,_) -> 
            if (v <= v1 && v <= v2 && chkHeapProperty hpL && chkHeapProperty hpR) then true
            else false 
  //    > chkHeapProperty ex3;;
  //  val it : bool = true
  //    > chkHeapProperty ex5;;
  //  val it : bool = false
  //    > chkHeapProperty empty;;
  //  val it : bool = true

(* Q1.3 *)
let rec map f h = 
    match h with
    | EmptyHP -> EmptyHP
    | HP(v,hpL,hpR) -> let root = f v       // Mapping in preOrder, since the heap property constraint:
                       let lt = map f hpL   // Intuitively it would seem useful to be able to handle the root node first
                       let rt = map f hpR   // if the root node defines the heap through the heap property
                       HP(root, lt, rt)

//   > map ((+)1) ex3;;
// val it : Heap<int> =
//  HP
//    (2,HP (3,HP (4,EmptyHP,EmptyHP),HP (6,EmptyHP,EmptyHP)),
//     HP (5,EmptyHP,EmptyHP))
//
// Exampel of manipulating ex3 with map, so it does not fulfill heap property:
let f = map (fun h -> if h < 4 then h + 5 else h - 4) ex3 // I was not able to do the 'chkHeapProperty (map f ex3)'; it throws:
                                                          // error FS0001: This expression was expected to have type
                                                          // ''a -> 'b'  but here has type 'Heap<int>'  
                                                          // If I do it like this, with f declared:   
                                                          //                                 
                                                          //    > chkHeapProperty f;;
                                                          //  val it : bool = false
(* Divide And Conquer *)
(* Q2.1 *)
let random =
  let rnd = System.Random()
  fun () -> rnd.Next(1,1000)  

  // @return array of random ints: val genRandoms : n:int -> int [] 
let genRandoms n = Array.init n (fun n -> random()) // Just using the Array.init with the above given function
let genRandomsP n = Array.Parallel.init n (fun n -> random()) // Just added .Parallel to 'genRandoms' solution
// example:   >  genRandomsP 5;;
//          val it : int [] = [|665; 176; 114; 154; 47|]

(* Q2.2 *)
(* Mergesort *)
// splits a list in two
let split xs = List.splitAt ((List.length xs)/2) xs // splits at the length of the list divided by two = midway(ish
// testcase1:   > split [22;746;931;975;200];;)
//            val it : int list * int list = ([22; 746], [931; 975; 200])
//
// testCase2:   > split [22;3434;342;6;756;867;97;6;456;7546;31;575;250;456;3;47;9];;
//            val it : int list * int list =
//              ([22; 3434; 342; 6; 756; 867; 97; 6],
//              [456; 7546; 31; 575; 250; 456; 3; 47; 9])
//
// testCase3:   > split [1];;
//            val it : int list * int list = ([], [1])
let indivisible xs = List.length xs = 1 || List.isEmpty xs
//    > indivisible [];;
//  val it : bool = true
//    > indivisible [1];;
//  val it : bool = true
//    > indivisible [1;2;3;4;5];;
//  val it : bool = false

// attempt at recursive declaration of merge:
(* let rec merge (xs, ys) =        // merge two sorted lists
    match (xs, ys) with
    | [], [] -> []        // changed implementation from CPS style, still get this error though  
    | xs, [] -> xs        // empty list merge
    | [], ys -> ys        // same
    | x::txs, y::tys -> 
      if (x<y) then x::(merge (txs, ys))  // checks which list's head is smallest, starts with that, and appends tail of same list, then other list.
      else y::(merge (xs, tys))           // no sorting like in the CPS implementation 
*)    

// attempt at CPS style implementation <- gives same results as above, tried to make it sort, no luck. 
let merge (xs, ys) =        // merge two sorted lists
  let rec mergeC c xs ys =  // trying to make a CPS implementation
     match xs, ys with
     | xs, [] -> c xs        // empty list merge
     | [], ys -> c ys        // same
     | hxs::txs, hys::tys -> 
       if hxs<hys then mergeC (fun acc -> c(hxs::acc)) txs ys  // list with smallest value at index 0 first append tail, then other list  
       else mergeC (fun acc -> c(hys::acc)) xs tys // viceversa
  mergeC (fun x -> x) xs ys       

// results for testing merge as implemented now:
// testCase1:   >  merge ([1;3;4;5],[1;2;7;9]);;
//            val it : int list = [1; 1; 2; 3; 4; 5; 7; 9]
//
// testCase2:   >  merge ([],[1;2;7;9]);;
//            val it : int list = [1; 2; 7; 9]  
//
// testCase3:   >  merge ([1;3;4;5],[]);;
//            val it : int list = [1; 3; 4; 5]
//
// testCase4:   >  merge ([3;1;4;5;9;2],[23;3;24;1;234;435]);;
//            val it : int list = [3; 1; 4; 5; 9; 2; 23; 3; 24; 1; 234; 435]  
//
// testCase5:   >  merge ([],[]);;
//            error FS0030: Value restriction.  
(* Q2.3 *)


let divideAndConquer s m i p = // @param s = split, m = merge, indivisible <- see implementations above
  let rec dc p =        // @param function dc and list p     
    match p with
    | p when (i p) -> p // if the list is indivisible, keep it.  
    | p            -> let l = s p // split the list give as the paramater p
                      m ((dc (fst l)), (dc (snd l))) // dc = sort split lists fst and snd, m = merge split lists
  dc p // from problem formulation
let divNCq p = divideAndConquer split merge indivisible p // passing above methods with + p, which is a list
// example :    > divNCq [22;746;931;975;200];;
//            val it : int list = [22; 200; 746; 931; 975]

(* Q3.1 *)
let triNum = Seq.initInfinite (fun n -> (n*(n + 1))/2) // using Seq.initInfinite as suggested with the given function for the sequence.
//    > triNum;;
//  val it : seq<int> = seq [0; 1; 3; 6; ...]

let triNumC = Seq.cache triNum // caching the sequence for better access. 
//    > triNumC;;
//  val it : seq<int> = seq [0; 1; 3; 6; ...]


(* Q3.2 *)
let rec filterOddIndex s =
  Seq.append (Seq.singleton (Seq.item 0 s))
             (filterOddIndex (Seq.skip 2 s))

let rec myFilterOddIndex s = 
  match s with 
  | s when Seq.empty = s -> failwith "empty Sequence" // added empty sequence case for good measure. Not rly working
  | s ->  Seq.append (Seq.singleton (Seq.item 0 s)) // same as given above
                     (Seq.delay (fun () -> myFilterOddIndex (Seq.skip 2 s))) // from reading seq api, 
//    > myFilterOddIndex triNum;;                                            // im attempting to use delay.  
//  val it : seq<int> = seq [0; 3; 10; 21; ...]                              // by adding Seq.delay with function call 
/// for exam:                                                                // to the skeleton given above.   
/// 3.2 not functioning. 
/// Still CPS version of divideAndConquer to add. 
/// does chkHeapProperty actually work? <- no, same EmptyHP cornercase <- needs to be tested after changing implementation
/// can you implement it without findfixed and comparator? <- solution? <- changed implementation, so i no longer use findfixed and the comparator. 
(* Q3.3 *)
let seqZip (s1: _ seq) (s2: _ seq) = seq { // this implementation needs annotation, if not cant use .GetEnumerator ()
    use e1 = s1.GetEnumerator () // e1 = sequence 1 index
    use e2 = s2.GetEnumerator () // e2 = sequence 2 index
    
    while e1.MoveNext () && e2.MoveNext () do // while sequence 1 and 2 hasNext
    yield e1.Current, e2.Current              // combine current index of sequence 1 with current index of sequence 2
}

(* Q4.1 *)
exception FigError of string
type Point = P of double * double

type Fig =
    Circle of Point * double 
  | Line of Point * Point
  | Move of double * double * Fig
  | Combine of Fig list
  | Label of string * Fig
  | Ref of string

//let rectEx = failwith "not implemented"

let rect (x1,y1) (x2,y2) = failwith "not implemented"

(* Q4.2 *)
let buildEnv fig = failwith "not implemented"

(* Q4.3 *)
let rec substFigRefs env fig = failwith "not implemented"

(* Q4.4 *)
let reduceMove fig = failwith "not implemented"

