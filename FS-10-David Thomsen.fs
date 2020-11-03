/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  10  ##############################
/// ###########################################################################
/// Author: David Thomsen <davt@itu.dk>
module a10

open System.Runtime.Remoting.Metadata.W3cXsd2001

    (* Exercise 1: *)
/// Run the slow Fibonacci computations from the lecture's
/// examples on your own computer.  Use the #time directive to turn on
/// timing, and see what is the best speed-up you can obtain for
/// computing, say, the first 43 Fibonacci numbers using Async.Parallel.

//  slow fibonacci implementation:
let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

let rec fibC (n:int) (c:float->float) = if n<2 then c 1.0 else fibC(n-2)(fun x -> fibC(n-1)(fun y -> c(x+y)));; 

//  computing the first 43 fibonacci numbers:
let fib43 = slowfib(43);;
//  speed test for computing the 43'th Fibonacci number:  
//  Real: 00:00:03.092, CPU: 00:00:03.093, GC gen0: 0, gen1: 0, gen2: 0
//  return@ single float.
let fibsl = [ slowfib(42); slowfib(43) ];;
//  speed test for computing the 42'th and 43'th Fibonacci number:
//  Real: 00:00:05.035, CPU: 00:00:05.046, GC gen0: 0, gen1: 0, gen2: 0
//  return@ list of floats: float list = [433494437.0; 701408733.0]
let fibsp = 
    let tasks = [ async { return slowfib(42)};
                  async { return slowfib(43)}]
    Async.RunSynchronously (Async.Parallel tasks);;
//  speed test for computing the 42'th and 43'th Fibonacci number, using Async.Parallel:
//  Real: 00:00:03.145, CPU: 00:00:05.093, GC gen0: 0, gen1: 0, gen2: 0
//  return@ list of floats: float list = [433494437.0; 701408733.0]  
let fibsi = [ for i in 0..43 do yield slowfib(i) ];;
//  speed test for computing the first 43 Fibonacci numbers:
//  Real: 00:00:08.067, CPU: 00:00:08.078, GC gen0: 0, gen1: 0, gen2: 0
//  return@ list of floats with all fibonacci numbers from 0 to 43.
let fibsip =
    let tasks = [ for i in 0..43 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
//  speed test for computing the first 43 Fibonacci numbers, using Async.Parallel:
//  Real: 00:00:03.507, CPU: 00:00:09.640, GC gen0: 0, gen1: 0, gen2: 0
//  return@ list of floats with all fibonacci numbers from 0 to 43.

/// The speed tests show that for single or few computed values, the results point to the Async.Parallel version, 
/// as the most efficient implementation. However, when testing for computing a long range of values, in this case 
/// 0...43, we get a substantial improvement on the Async.Parallel implementation in real time, altough it requires 
/// slightly more of CPU time, compared to the clean 'for i' implementation. In conclusion to exercise  one,
/// it is evident, that an implementation using Async.Parallel achieves the best speed-up on my hardware, for computing
/// Fibonacci numbers. 

    (* Exercise 2: *)
/// Similarly, run the prime factorization example on your own
/// computer, and see what speedup you get.  

//  Checks if a number n is a prime number. 
//  param@ an int n
//  return@ boolean false if n is not prime, boolean true if n is prime
let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;
//  computes the prime factors of a number n.
//  param@ a number n
//  return@ list of ints, being the prime factors of a number n. 
let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;
//  generates n random ints
//  param@ a number n
//  return@ n pairs of ('unit', int)     
let random n =
    let generator = new System.Random ()
    fun () -> generator.Next n;;
//  10000 randoms ints using 'random' with n = 10000
//  return@ 10000 pairs of (unit, int): val r10000 : (unit -> int)
let r10000 = random 10000;;
let rec ntimes (f : unit -> 'a) n =
    if n=0 then () else (ignore (f ()); ntimes f (n-1));;
//  initializes Array for testing using 'r10000' above
let bigArray = Array.init 5000000 (fun _ -> r10000 ());;

//  computing if a number is Prime and prime factorization of a number and a range of numbers:  
Array.map isPrime bigArray;;
//  speed test for checking if every indexed number is prime in an Array of size n = 5000000.
//  Real: 00:00:00.256, CPU: 00:00:00.250, GC gen0: 0, gen1: 0, gen2: 0
//  return@ an Array of true/false. 
Array.Parallel.map isPrime bigArray;;
//  speed test for checking if a number is prime in an Array of size n = 5000000, using Array.Parallel.
//  Real: 00:00:00.053, CPU: 00:00:00.375, GC gen0: 0, gen1: 0, gen2: 0
//  return@ an Array of true/false.
Array.map factors bigArray;;
//  speed test for computing prime factors of every index of 'bigArray'.
//  Real: 00:00:15.739, CPU: 00:00:16.515, GC gen0: 47, gen1: 39, gen2: 5
//  return@ an Array of int lists, where each list contains the prime factors of a number from 'bigArray'.
Array.Parallel.map factors bigArray;;
//  speed test for computing prime factors of every index of 'bigArray', using Array.Parallel.
//  Real: 00:00:04.639, CPU: 00:00:25.343, GC gen0: 43, gen1: 42, gen2: 1
//  return@ an Array of int lists, where each list contains the prime factors of a number from 'bigArray.
Array.init 200000 factors;;
//  speed test for computing prime factors of a range of number from 1..200000.
//  Real: 00:00:08.200, CPU: 00:00:08.203, GC gen0: 2, gen1: 1, gen2: 0
//  return@ an Array of int lists, where each list contains the prime factors of a number in the range 1..200000.
let factors200000 = Array.Parallel.init 200000 factors;;
//  speed test for computing prime factors of a range of number from 1..200000, using Array.Parallel.
//  Real: 00:00:01.691, CPU: 00:00:12.765, GC gen0: 2, gen1: 1, gen2: 0
//  return@ an Array of int lists, where each list contains the prime factors of a number in the range 1..200000.

/// In conclusion to exercise 2, it is evident from the speed tests, that the trend decribed in exercise 1 continues.
/// Using the .Parallel design achieves a significant improvement on real time at the expence of a small increase in
/// CPU time. The gains from this, seems to increase, the heavier the task required. This leads me to conclude, that 
/// using a .Parallel design approach for the heavy lifting in a program, while using a more idiomatic approach (sequential)
/// for the simple tasks, will provide an efficient trade of between real time and CPU usage. 


    (* Exercise 3: *) /// not done
/// The lecture's construction of a histogram (counting the
/// numbers of times that each prime factor 2, 3, 5, 7, 11 ... appears)
/// uses a side effect in the assignment 

///     histogram.[i] <- histogram.[i] + 1 

/// But side effects should be avoided.  Program the histogram
/// construction so that it does not use side effects but purely
/// functional programming.  There are several useful functions in the Seq
/// module.  The final result does not have to be an int[] array, but
/// could be a seq<int * int> of pairs (p, n) where p is a prime factor
/// and n is the number of times p appears in the array of lists of prime
/// factors.

let histogram = Seq.init 200000 (fun i -> 0)
let incr i = Seq.countBy 

Array.iter (fun fs -> List.iter incr fs) factors200000;;
    
    (* Exercise 4: *) /// not done
/// Find the fastest way on your hardware to count the number
/// of prime numbers between 1 and 10 million (the correct count is
/// 664579).
