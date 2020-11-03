/// ###########################################################################
/// ######################### Functional  Programming #########################
/// #############################  Assignment 12  #############################
/// ###########################################################################
/// Author: David Thomsen <davt@itu.dk> 

//  I hereby declare that I myself have created this exam hand-in in its entirety without help from anybody except the TAs.

module a12
(* Question 1.1 *)

// helper methods: 

type Heap<'a when 'a: equality> = 
  | EmptyHP
  | HP of 'a * Heap<'a> * Heap<'a>

// declaring ex3 

let ex1 = EmptyHP
let ex2 = HP(13, HP(9, HP(3, EmptyHP, EmptyHP), 
                 HP(4, EmptyHP, EmptyHP)),
                       HP(5, EmptyHP, EmptyHP))
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), 
                HP(4, EmptyHP, EmptyHP)),
                      HP(5, EmptyHP, EmptyHP))
let _ = ex3 : Heap<int>

// polymorphism vs monomorphism 

(* The type of ex3 is int (Heap<int>) which is a monomorphic type. Since it is an instance of the polymorphic datastructure (Heap) where the stored values are integers  *)

// declaring empty Heap. 

let empty = EmptyHP
let _ = empty : Heap<'a> when 'a : equality

// declaring HeapError exception.  

exception HeapError of string 

(* Question 1.2 *)

// declaring function isEmpty
let isEmpty = function
    | EmptyHP   -> true
    | _         -> false 
let _ = isEmpty : Heap<'a> -> bool when 'a : equality

// declaring size 
let rec size h = 
    match h with 
    | EmptyHP       -> 0 
    | HP(_,hp1,hp2) -> 1 + size hp1 + size hp2 
let _ = size : Heap<'a> -> int when 'a : equality 

// declaring chkHeapProperty
let rec chkHeapProperty h =  
    match h with 
    | EmptyHP        -> true 
    | HP(v,hpL,hpR)  -> 
         match hpL, hpR with
         | EmptyHP, EmptyHP       -> true 
         | HP(v1,_,_), HP(v2,_,_) ->
            (v <= v1 && v <= v2 && chkHeapProperty hpL && chkHeapProperty hpR)  
         | _                      -> raise (HeapError "unexpected cornercase")   
let _ = chkHeapProperty : Heap<'a> -> bool when 'a : comparison

let myAssert a = 
    let res = a 
    if res then () else raise (HeapError "assertion failed")

// declaring find. 
// I switched the implementations of find and chkHeapProperty, so I can use chkHeapProperty to confirm, that the Heap property is intact. 
// If the Heap property is intact, I can just return the root of the Heap, since we know a priori from the Heap property,
// that the root is the lowest value stored. 
let find h =
    myAssert (chkHeapProperty h)   
    match h with 
    | EmptyHP       -> raise (HeapError "heap is empty")
    | HP(n,hpl,hpr) -> n
let _ = find : Heap<'a> -> 'a when 'a : equality

(* Question 1.3 *) 

// declaring map
let rec map f h = 
    match h with
    | EmptyHP -> EmptyHP
    | HP(v, hpL, hpR) -> let root = f v
                         let travLeft = map f hpL
                         let travRight = map f hpR
                         HP(root, travLeft, travRight)
let _ = map : ('a -> 'b) -> Heap<'a> -> Heap<'b> when 'a : equality and 'b : equality 

// Traversing the datastructure in preOrder. It seems intuitiv for the Heap datastructure, to use this inorder traversal, 
// since we have the heap property as a constraint, therefore starting at the root node would make sense, however the design of the nodes in the datastructure (v,leftNode,rightNode)
// makes preOrder my choice, as i wont have to rearrange the node structure when applying f to all values.  

// declaring f 
let f = (fun h -> if h < 5 then h + 5 else h - 5)
(*
> chkHeapProperty ex3;;
val it : bool = true

> chkHeapProperty (map f ex3);;
val it : bool = false
*)

(* Question 2.1 *)

// helper definitions

let random = 
    let rnd = System.Random()
    fun () -> rnd.Next(3, 10000) // not the biggest fan of including 1 and 2s 

let rec power = function
    | (_, 0) -> 1
    | (x, n) -> x * power(x, n-1)

// declaring genRandom
let genRandoms n = Array.init n (fun n -> random())
let _ = genRandoms : int -> int[]

// example
let ex4 = genRandoms 10

// declaring genRandomsP
let genRandomsP n = Array.Parallel.init n (fun n -> random())

// example
let ex5 = genRandomsP 20000
let ex6 = genRandomsP 10

(* Question 2.2 *)
(* Dear TA (Jonas): Concerning Question 2.2, if it is too much of a mess, and you feel you cannot give me proper feedback on the actual question, just ignore 2.2 in its entirety, I do not need the points   *)
(* Any feedback on my different implementations of merge will be well recieved though. I do not intend to do anything similar at the exam, this was an opportunity to practice and test the theory.           *)
(* The implementation named "merge" is my final solution. I realized that using List.sort was kinda not implementing the algorithm at all. The final merge implementation achieves similar results as mergeA. *)
(* Kind regards, David.                                                                                                                                                                                       *)


// declaring split
let split xs = 
    match xs with
    | []       -> ([],[])
    | xsH::xsT -> List.splitAt ((List.length  xs) / 2) xs
let _ = split : 'a list -> 'a list * 'a list 

// test cases: 
let splitex1 = [] : int list                    // how does split handle the empty list    
(*
> split splitex1;;
val it : int list * int list = ([], [])
*)
// splits the empty list into two empty lists, not sure this is correct though, might also implement to throw error. 
// In terms of the mergeSort algorithm, we cover the cases of empty lists or singleton lists in the "indivisible" implementation 

let splitex2 = genRandomsP 10 |> List.ofArray   // how does split handle an even length list (List.length % 2 = 0)   
(*
> split splitex2;;
val it : int list * int list =
  ([1644; 1471; 4298; 6273; 1291], [9555; 3545; 8499; 4990; 2515])
*)
// as expected, the list is divided into two equally long lists (notice no sorting).

let splitex3 = genRandomsP 11 |> List.ofArray   // how does split handle an uneven length list (List.length % 2 != 0)
(*
> split splitex3;;
val it : int list * int list =
  ([9734; 5146; 7476; 4306; 923], [1915; 9545; 6889; 7769; 563; 7171])
*)
// as expected, the list is divided into two lists, not of equal size (again, notice no sorting).

// declaring indivisible 
let indivisible xs =
    let listLength = (List.length xs)
    match xs with
    | []                             -> true
    | [_] when listLength = 1        -> true
    | _                              -> false 
let _ = indivisible : 'a list -> bool 

// declaring merge 
let merge (xs, ys) = // final tailrecursive implementation of merge, not using List.sort. I could (should) erase all the other implementations, and the testing of them, 
    let newList =    // but that work got me to this version, so I am keeping it for future clarifications (panic at the exam). I do apologize for the mess.    
        let rec loop xs ys acc =
            match xs, ys with
            | [], []                           -> acc 
            | hlst::tlst, [] | [], hlst::tlst  -> loop tlst [] (hlst::acc)
            | hxs::txs, hys::tys               -> if hxs < hys then loop txs ys (hxs::acc) else loop xs tys (hys::acc) 
        loop xs ys []
    List.rev newList    
let _ = merge : 'a list * 'a list -> 'a list when 'a : comparison

(*
> merge mergeTest;;
val it : int list = [1; 1; 2; 3; 4; 5; 7; 9]
*)

// Is using List.sort maybe not so cool? might as well answer the whole question 2, by just sorting a list. No surprise if the library function uses mergeSort like java's does.
let merge1 (xs, ys) = // notice the sorting here.  
    let unSortedList =
        let rec loop xs ys = 
            match xs, ys with
            | [], []             -> []
            | xs, []             -> xs
            | [], ys             -> ys
            | xsH::xsT, ysH::ysT -> xsH::(loop xsT ys)
        loop xs ys
    List.sort unSortedList     
let _ = merge1 : 'a list * 'a list -> 'a list when 'a : comparison  

let merge2 (xs, ys) = // so I made one that does try to "sort" itself. With no success 
    let newList =
        let rec loop acc xs ys = 
            match xs, ys with
            | [], [] -> acc
            | xs, [] -> acc @ xs
            | [], ys -> acc @ ys
            | hxs::txs, hys::tys -> 
                if hxs < hys then loop (hxs::acc) txs ys  // these two lines might not sort a longer list correctly, however, we should only be dealing with singleton lists when using merge in divideAndConquer
                else loop (hys::acc) xs tys               // It does, however, not sort even in that case. Problem being second and third case, naively appending the remaining list to the acc. See final merge implementation for solution
        loop [] xs ys
    List.rev newList
let _ = merge2 : 'a list * 'a list -> 'a list when 'a : comparison  

let mergeTest = ([1;3;4;5],[1;2;7;9])
// test cases:

let ex7 = (genRandomsP 10) |> List.ofArray |> split // can it merge (and sort) two lists of equal size? 
(*
> merge ex7;;
>
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int list =
  [604; 1450; 1897; 2928; 3480; 3684; 6134; 6522; 7112; 8927]
*)
// does a great job

let ex8 = (([]: int list), (genRandomsP 10) |> List.ofArray) // merging one empty list with a non empty list.
(*
> merge ex8;;
>
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int list =
  [526; 2412; 2706; 5910; 6401; 7942; 8010; 8320; 8519; 8864]
*)
// no problem

let ex9 = ((genRandomsP 10) |> List.ofArray, ( []: int list)) // other way around too?
(*
> merge ex9;;
>
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int list =
  [1384; 1398; 2643; 3225; 4551; 7612; 8048; 8329; 8794; 9251]
*)
// yes

(* next part is not required in the assignment, but I got curious *)
let ex10 = (genRandomsP (power (10, 4))) |> List.ofArray |> split // careful here - how does merge handle larger sized lists?
(*
> merge ex10;; <- This implementation is now named merge1. 
>
Real: 00:00:00.000, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it : int list =
  [6; 6; 7; 8; 9; 10; 12; 12; 13; 15; 15; 16; 18; 19; 21; 22; 25; 26; 30; 31;
   31; 32; 32; 34; 34; 35; 35; 36; 37; 38; 38; 39; 40; 41; 42; 43; 46; 48; 50;
   52; 53; 53; 55; 56; 56; 57; 57; 58; 58; 59; 60; 60; 61; 61; 66; 66; 68; 69;
   71; 74; 75; 75; 75; 76; 76; 77; 77; 77; 79; 80; 81; 81; 82; 82; 82; 83; 83;
   84; 85; 85; 85; 86; 86; 87; 87; 88; 88; 90; 92; 93; 93; 94; 94; 94; 94; 95;
   96; 96; 98; 98; ...]
*)

// lets look at an implementation using CPS: 
let mergeC (xs, ys) =
    let unSortedList =
        let rec loop c xs ys =  
            match xs, ys with
            | xs, []             -> c xs       
            | [], ys             -> c ys       
            | hxs::txs, hys::tys -> loop (fun acc -> c(hxs::acc)) txs ys    
        loop id xs ys 
    List.sort unSortedList                             
let _ = mergeC : 'a list * 'a list -> 'a list when 'a : comparison  

(*
> mergeC ex10;;
> Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int list =
  [6; 6; 7; 8; 9; 10; 12; 12; 13; 15; 15; 16; 18; 19; 21; 22; 25; 26; 30; 31;
   31; 32; 32; 34; 34; 35; 35; 36; 37; 38; 38; 39; 40; 41; 42; 43; 46; 48; 50;
   52; 53; 53; 55; 56; 56; 57; 57; 58; 58; 59; 60; 60; 61; 61; 66; 66; 68; 69;
   71; 74; 75; 75; 75; 76; 76; 77; 77; 77; 79; 80; 81; 81; 82; 82; 82; 83; 83;
   84; 85; 85; 85; 86; 86; 87; 87; 88; 88; 90; 92; 93; 93; 94; 94; 94; 94; 95;
   96; 96; 98; 98; ...]
*)
// so far so good. LARGER sizes then?

(* will stackoverflow above merge implementation.*)
let ex11 = (genRandomsP (power (10, 6))) |> List.ofArray |> split // VERY careful here
let ex12 = (genRandomsP (power (10, 8))) |> List.ofArray |> split // GOOD LUCK 

(*
val ex11 : int list * int list =
  ([5027; 8164; 4129; 9299; 3910; 8753; 4577; 9834; 7247; 195; 5937; 6268;
    8961; 5774; 3075; 2077; 2880; 4695; 9035; 7446; 9325; 2024; 5195; 6214;
    2188; 9373; 3304; 4403; 5533; 4135; 7477; 7538; 3150; 333; 6692; 9913;
    5705; 6570; 5479; 3902; 9087; 2556; 3436; 4399; 2156; 1747; 5388; 4366;
    9260; 503; 2084; 2552; 8962; 7699; 9719; 3006; 2972; 7914; 7113; 4537;
    5452; 177; 4303; 3115; 2718; 8398; 3121; 8631; 9082; 3161; 6372; 6309;
    9215; 5136; 8358; 6772; 8587; 798; 4061; 444; 3987; 8937; 5143; 5032; 2054;
    4927; 8576; 5451; 613; 3688; 6943; 7790; 9457; 944; 8450; 8913; 8252; 324;
    1684; 3758; ...],
   [8232; 9126; 4243; 7653; 6384; 6980; 7526; 9593; 1071; 3770; 2224; 1182;
    8229; 1622; 4871; 1298; 2807; 6371; 1796; 6643; 2444; 7363; 2416; 322;
    1834; 3315; 8035; 6353; 3322; 8081; 9303; 6750; 6932; 9795; 5342; 869;
    4468; 1358; 1173; 21; 101; 1276; 6625; 9491; 9598; 774; 1856; 5313; 3718;
    1174; 8246; 5121; 8230; 7602; 5901; 6666; 7140; 8824; 7267; 4463; 5812;
    3997; 6974; 4688; 5312; 2517; 3067; 4804; 2792; 9045; 7410; 6742; 7777;
    8613; 8529; 3831; 4725; 7515; 1162; 2926; 7025; 3640; 4397; 9244; 6643;
    5123; 6824; 1111; 3926; 1778; 6283; 1794; 5107; 6770; 4833; 3472; 6786;
    2319; 624; 9454; ...])

    > mergeC ex11;;
    Real: 00:00:14.589, CPU: 00:00:14.593, GC gen0: 1021, gen1: 352, gen2: 1
    val it : int list =
      [3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
       3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
       3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
       3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
       ...]
*)
// not stackoverflow, since using CPS style, but slow. Can we speed it up with a tailrecursive implementation?

let mergeA (xs, ys) = 
    let unSortedList = 
        let rec loop acc xs ys = 
            match xs, ys with 
            | xs, []             -> acc @ xs     
            | [], ys             -> acc @ ys       
            | hxs::txs, hys::tys -> loop (hxs::acc) txs ys 
        loop [] xs ys
    List.sort unSortedList 
let _ = mergeA : 'a list * 'a list -> 'a list when 'a : comparison
(*
val ex11 : int list * int list =
  ([1415; 1331; 1335; 3722; 7738; 8471; 1722; 4832; 1787; 3553; 1542; 9370;
    756; 1907; 7539; 5255; 6314; 7003; 6874; 8969; 6684; 4584; 9409; 523; 340;
    5514; 6835; 1826; 1682; 1077; 8034; 906; 668; 1146; 4232; 6760; 2729; 9549;
    6885; 7689; 7322; 4673; 9862; 3075; 1399; 4166; 9295; 8496; 8718; 4136;
    6288; 4910; 7535; 7208; 567; 9059; 9733; 257; 3300; 2819; 7072; 7327; 7489;
    8072; 9057; 4004; 4656; 1684; 3433; 7234; 7677; 2183; 4917; 2840; 7579;
    7157; 2550; 8296; 4501; 2987; 3131; 4950; 7775; 2092; 1427; 7776; 5218;
    3833; 3340; 3656; 6159; 7702; 8725; 4895; 5204; 4258; 90; 6996; 7681; 8157;
    ...],
   [2384; 8911; 345; 1901; 631; 1206; 6840; 6864; 191; 2336; 1952; 719; 3840;
    3685; 2632; 4774; 4743; 2973; 5078; 9241; 5953; 2841; 115; 7731; 6581; 349;
    5754; 7826; 5217; 2352; 1166; 158; 5433; 5936; 6469; 7747; 626; 3893; 6920;
    4094; 6879; 5244; 9183; 9876; 2235; 1880; 469; 7666; 2170; 8771; 9382;
    2854; 3108; 5194; 9533; 7741; 7519; 418; 3309; 3176; 438; 199; 7558; 2656;
    9569; 4285; 6959; 3119; 1202; 9383; 181; 2836; 1253; 1394; 3187; 5570;
    1995; 1141; 4302; 3431; 4432; 9581; 6513; 917; 7719; 9848; 5686; 9558;
    9847; 7139; 7322; 7906; 69; 8723; 3964; 2220; 8203; 7415; 5252; 5641; ...])

    > List.rev (mergeA ex11);;
    Real: 00:00:00.141, CPU: 00:00:00.140, GC gen0: 15, gen1: 5, gen2: 0
    val it : int list =
      [9997; 9997; 9997; 9996; 9995; 9995; 9995; 9994; 9992; 9990; 9990; 9987;
       9987; 9987; 9987; 9987; 9986; 9985; 9981; 9981; 9981; 9981; 9981; 9980;
       9977; 9977; 9975; 9975; 9975; 9975; 9974; 9974; 9974; 9974; 9974; 9974;
       9973; 9973; 9966; 9965; 9961; 9961; 9957; 9957; 9956; 9956; 9956; 9956;
       9955; 9955; 9954; 9951; 9950; 9950; 9950; 9949; 9947; 9947; 9944; 9944;
       9942; 9941; 9940; 9940; 9940; 9939; 9939; 9937; 9937; 9937; 9935; 9932;
       9931; 9931; 9930; 9930; 9930; 9928; 9928; 9921; 9921; 9920; 9918; 9918;
       9916; 9915; 9915; 9914; 9913; 9913; 9913; 9912; 9911; 9910; 9910; 9910;
       9908; 9906; 9905; 9904; ...]
*)
// very impressive performance from the tailrecursive implementation indeed (using List.rev to not only get visible 3's as above when comparing mergeC)
// However, all this is kinda for non, since the List.sort call in the mergeC and mergeA is properly what makes it perform so well. 
// The final solution of merge (as it is now), however, does perform competitively with mergeA, not getting stackoverflow on a random generated list of 10^6 integers and achieving similar times when passed to divideAndConquer

(* Question 2.3 *)

let divideAndConquer split merge indivisible p =
    let rec dc p = 
        if indivisible p 
            then p
            else let l = split p
                 merge ((dc (fst l)),(dc (snd l)))
    dc p            
let _ = divideAndConquer : ('a -> 'a * 'a) -> ('a * 'a -> 'a) -> ('a -> bool) -> 'a -> 'a

let dCTest = [22;746;931;975;200]
(*
> divideAndConquer split merge indivisible dCTest;;
val it : int list = [22; 200; 746; 931; 975]
*)
let ex13 = (genRandomsP (power (10, 4))) |> List.ofArray
let ex14 = (genRandomsP (power (10, 6))) |> List.ofArray
let ex15 = (genRandomsP (power (10, 8))) |> List.ofArray
(*
val ex14 : int list =
  [6263; 1729; 4010; 459; 5003; 9403; 2226; 5595; 4419; 2381; 176; 3186; 2099;
   8649; 5065; 6612; 6027; 5879; 2100; 509; 8455; 9053; 8716; 5463; 3252; 1716;
   7659; 61; 1701; 7311; 8288; 4053; 6914; 3419; 8470; 1348; 7971; 9704; 8878;
   3857; 4029; 6719; 3589; 1556; 4503; 1371; 4201; 5359; 564; 4423; 9905; 992;
   3903; 8668; 9543; 4165; 1222; 5554; 1405; 6287; 3942; 8974; 3882; 6760;
   2323; 8475; 5875; 3810; 4598; 8150; 3195; 7557; 4534; 4129; 804; 9576; 5199;
   4689; 8744; 9662; 162; 3159; 8689; 7499; 1955; 7726; 9630; 7009; 2430; 4569;
   2679; 8427; 5542; 7658; 8302; 2626; 435; 9647; 2582; 623; ...]

> List.rev (divideAndConquer split merge indivisible ex14);;
Real: 00:00:01.538, CPU: 00:00:01.515, GC gen0: 370, gen1: 54, gen2: 0
val it : int list =
  [9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999;
   9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999;
   9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9998;
   9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998;
   9998; 9998; 9998; 9998; 9998; 9997; 9997; 9997; 9997; 9997; 9997; 9997;
   9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997;
   9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997;
   9997; 9997; 9997; 9997; 9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996;
   9996; 9996; 9996; 9996; ...]
*)

(*
val ex14 : int list =
  [870; 3489; 9613; 7181; 2707; 1730; 7769; 4569; 6984; 516; 8847; 7256; 5010;
   1628; 6350; 8225; 1179; 2993; 9634; 9434; 5183; 3967; 112; 522; 6941; 4975;
   2942; 7922; 2402; 177; 3210; 4211; 3526; 2426; 572; 9524; 100; 8491; 1787;
   9587; 654; 6178; 6904; 2064; 7089; 1366; 9351; 7807; 3770; 457; 7015; 940;
   8009; 4003; 7327; 218; 7311; 2711; 5119; 5617; 366; 8418; 6762; 3217; 62;
   1835; 6318; 7001; 7624; 9022; 8009; 3867; 284; 4517; 3820; 4819; 5548; 3350;
   7304; 6881; 3143; 6623; 924; 4777; 1154; 5201; 346; 3245; 7908; 6751; 4707;
   4552; 5143; 4482; 2709; 7511; 9554; 5982; 7287; 5938; ...]

> List.rev (divideAndConquer split mergeA indivisible ex14);; 
Real: 00:00:01.110, CPU: 00:00:01.109, GC gen0: 281, gen1: 48, gen2: 0
val it : int list =
  [9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999;
   9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999; 9999;
   9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998; 9998;
   9998; 9998; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997; 9997;
   9997; 9997; 9997; 9997; 9997; 9997; 9996; 9996; 9996; 9996; 9996; 9996;
   9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996; 9996;
   9995; 9995; 9995; 9995; 9995; 9995; 9995; 9995; 9995; 9994; 9994; 9994;
   9994; 9994; 9994; 9994; 9994; 9994; 9994; 9994; 9994; 9994; 9994; 9994;
   9994; 9994; 9993; 9993; ...]
*)
// seems to work really nice. 
// What could be really interesting now, is considering whether this divideAndConquer algorithm really achieves O(n log(n)) amortized time, as it theoretically should, 
// and if so, what is the difference between the CPS, tailrecursion and naive implementation? Do they all achieve the theoretical threshold? if not, why not?
// BUT: I am not going to do that :( - I will, however, provide a reference for my postulations: https://www.khanacademy.org/computing/computer-science/algorithms/merge-sort/a/analysis-of-merge-sort

(* Question 3.1 *)

// declaring triNum

let triNum = Seq.initInfinite (fun n -> (n*(n+1))/2)
let _ = triNum : seq<int>

// declaring triNum 
let triNumC = Seq.cache triNum
let _ = triNumC : seq<int>

(* Question 3.2*)

// declaring myFiltyerOddIndex 

let myFilterOddIndex s =
    let rec loop s =
        match s with
        | s when Seq.empty = s -> failwith "empty sequence"
        | s                    -> Seq.append (Seq.singleton (Seq.item 0 s))
                                             (Seq.delay (fun () -> loop (Seq.skip 2 s))) // really just added Seq.delay with the recursive call in a function, as described in HR 11.3
    loop s                
let _ = myFilterOddIndex : seq<'a> -> seq<'a>
(*
> myFilterOddIndex triNumC;;
val it : seq<int> = seq [0; 3; 10; 21; ...]
*)

(* Question 3.3 *)

// declaring seqZip 

let seqZip s1 s2 = 
    let rec loop s1 s2 = 
        seq { let e1 = Seq.item 0 s1
              let e2 = Seq.item 0 s2  
              yield (e1,e2)                           // add tuples of e1 and e2 to the sequence          
              yield! loop (Seq.tail s1) (Seq.tail s2) // recursively append rest of the sequences s1 and s2 
            }
    loop s1 s2 
let _ = seqZip : (seq<'a> -> seq<'b> -> seq<'a * 'b>)

(* 
> seqZip triNum triNum;;
val it : seq<int * int> = seq [(0, 0); (1, 1); (3, 3); (6, 6); ...]
*)

// helper definitions for question 4, provided in assignment. 

exception FigError of string

type Point = P of double * double

type Fig =
    Circle of Point * double 
  | Line of Point * Point
  | Move of double * double * Fig
  | Combine of Fig list
  | Label of string * Fig
  | Ref of string

(* Question 4.1 *)

let rectEx = 
    Combine  [
             Line(P(-1.0, 1.0), P(-1.0, -1.0)); 
             Line(P(-1.0, -1.0), P(1.0, -1.0)); 
             Line(P(1.0, -1.0), P(1.0, 1.0));
             Line(P(1.0, 1.0), P(-1.0, 1.0));
             ]
let _ = rectEx : Fig 

let rect (x1,y1) (x2,y2) = // this is slight awkward with the pattern matchin on one case. But it was the way it would match the type constraint.  (double * double) * (double * double) -> Fig  
    match (x1,y1), (x2, y2) with
    | (x1,y1), (x2,y2) -> Combine [
                                  Line(P(x1, y1), P(x1, y2)); 
                                  Line(P(x1, y2), P(x2, y2)); 
                                  Line(P(x2, y2), P(x2, y1));
                                  Line(P(x2, y1), P(x1, y1));                            
                                  ]                        
let _ = rect : double * double -> double * double -> Fig 

(*
> rect (-2.0,1.0) (1.0,-1.0);;
val it : Fig =
  Combine
    [Line (P (-2.0,1.0),P (-2.0,-1.0)); Line (P (-2.0,-1.0),P (1.0,-1.0));
     Line (P (1.0,-1.0),P (1.0,1.0)); Line (P (1.0,1.0),P (-2.0,1.0))] 
*)
let examTestExample = rect (-2.0,1.0) (1.0,-1.0) = Combine [Line(P(-2.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));Line(P(1.0,-1.0),P(-2.0,-1.0));Line(P(-2.0,-1.0),P(-2.0,1.0))]  
// so this returns false, however, I believe that is because I am "drawing" the rectangle in the opposite direction. Please provide proper feedback on this, and not just some scripted error on 4.1 :)

(* Question 4.2 *)

let environment = Map.empty

let buildEnv = function
    | Combine (Label (string, fig)::rest) -> environment.Add(string, fig)
    | _                                   -> raise (FigError "figure not recognized by DSL, see line 510")
let _ = buildEnv : Fig -> Map<string, Fig>

let figEx02 =
    Combine [Label("c",Circle(P(0.0,0.0),1.0));
             Move(1.0,1.0,Ref "c");
             Move(2.0,2.0,Ref "c")]

let envEx02 = buildEnv figEx02 // val envEx02 : Map<string,Fig> = map [("c", Circle (P (0.0,0.0),1.0))] 

(* Question 4.3 *)
let substFigRefs env fig = 
    let rec loop (env: Map<string, Fig>) fig acc = 
        match fig with 
        | Combine (Label (string, fig)::rest)      -> loop env (Combine rest) (fig::acc)
        | Combine (Move (x, y, Ref refKey)::rest)  -> if env.ContainsKey refKey 
                                                      then loop env (Combine rest) (Move (x, y, Map.find refKey env)::acc) 
                                                      else raise (FigError "environment does not contain reference key, see line 553 - 561")
        | Combine (hxs::txs)                       -> loop env (Combine txs) (hxs::acc)
        | Combine ([])                             -> List.rev acc
        | _                                        -> raise (FigError "Illegal figure, or figure not accounted for, see line 569 - 575")
    Combine (loop env fig [])
let _ = substFigRefs : Map<string,Fig> -> Fig -> Fig 

let substEx02 = substFigRefs envEx02 figEx02

let examTestExample1 = substEx02 =                                                      // val examTestExample1 : bool = true <- only cause of the List.rev call in line 574
                                   Combine [ Circle(P(0.0,0.0),1.0);
                                             Move(1.0,1.0,Circle(P(0.0,0.0),1.0));
                                             Move(2.0,2.0,Circle(P(0.0,0.0),1.0)) ]

(* Question 4.4 *)
let reduceMove fig = 
    let rec loop fig acc = 
        match fig with 
        | Combine (Move (x, y, Circle (P (x', y'), radius))::rest)    -> loop (Combine rest) (Circle (P(x+x',y+y'), radius)::acc)
        | Combine (Move (x, y, Line (P (x', y'), P(x'', y'')))::rest) -> loop (Combine rest) (Line (P (x + x', x + y'), P(x + x'', x + y''))::acc)
        | Combine (hxs::txs)                                          -> loop (Combine txs) (hxs::acc)
        | Combine ([])                                                -> List.rev acc 
        | _                                                           -> raise (FigError "Illegal Move, or Move not accounted for, see line 590 - 594")
    Combine (loop fig [])
let _ = reduceMove : Fig -> Fig 


let reduceEx02 = reduceMove substEx02 

let examTestExample2 = reduceEx02 =                                                     // val examTestExample2 : bool = true
                                    Combine [Circle(P(0.0,0.0),1.0);
                                             Circle(P(1.0,1.0),1.0);
                                             Circle(P(2.0,2.0),1.0)]
(* END OF EXAM ASSIGNMENT*)