/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  11 (Garbage collection)  ########
/// ###########################################################################
/// Author: David Thomsen <davt@itu.dk>

module QueueWithMistake
// Lock-based queue with memory management mistake
// sestoft@itu.dk * 2013-10-20
// nh@itu.dk - 2019-03-31:
//   Concurrency removed from example.
//   Now just a simple array with queues uesed.
//   Text below edited accordingly.
//   Translated from Java to F#.

// The SentinelLockQueue is a simple first-in first-out buffer,
// implemented as a linked list of Nodes.  Method queue.put(x) inserts
// item x and method queue.get() removes and returns an item provided
// the queue is not empty.

// The queue implementation in class SentinelLockQueue has an extra
// "sentinel" Node so that the head and tail fields always have a Node
// to point to, even when the queue is empty.  This means that the
// first integer item in the queue is not head.item but head.next.item.

// The queue implementation contains a programming mistake that
// usually causes the test program to run out of memory quite soon
// although this should not happen.

// The mistake actually appears in an example in Goetz et al: Java
// Concurrency in Practice, 2006, page 334 (ch 15) and the book's
// errata and online source code do not correct it.  But in all other
// respects it is an extremely useful and recommendable book!

type Node(item:int, next:Node option) =
  let mutable _item = item
  let mutable _next = next

  member this.Item
    with get() = _item
    and  set(v) = _item <- v
  member this.Next
    with get() = _next    
    and  set(v) = _next <- v

// --------------------------------------------------
// Locking queue, with sentinel (dummy) node

type SentinelLockQueue() =
  // With sentinel (dummy) node.
  // Invariants:
  //  * The node referred by tail is reachable from head.
  //  * If non-empty then head != tail, 
  //     and tail points to last item, and head.next to first item.
  //  * If empty then head == tail.

  let dummy = Node(-444,None)
  let mutable head = dummy
  let mutable tail = dummy

  member this.put(item) =
    let node = Node(item, None)
    tail.Next <- Some node;
    tail <- node

  member this.get() =
    if (head.Next = None)
      then -999
      else
        let first = head
        head <- first.Next.Value;
        first.Next <- None // makes first.next point to null, making garbage collection possible. 
        head.Item

let time f =
  let start = System.DateTime.Now in
  let res = f () in
  let finish = System.DateTime.Now in
  (res, finish - start)

let run() =
  let iterations = 600000
  let noQueues = 60
  let queues = Array.init noQueues (fun _ -> SentinelLockQueue())
  let doIter j =
    queues.[j].put(42);
    for i in 0 .. iterations-1 do
      queues.[j].put(i);
      queues.[j].get()
    |> ignore
  for j in 0 .. noQueues-1 do
    let (_,t) = time (fun () -> doIter j)
    printfn "Qno. %2d\t %10d %A\n" j (queues.[j].get()) t
(*exercise 1*)
// running the original java code (QueueWithMistake) gives:
(*
SentinelLockQueue       Qno.  0    2,44 19999999
SentinelLockQueue       Qno.  1    4,92 19999999
SentinelLockQueue       Qno.  2    7,37 19999999
SentinelLockQueue       Qno.  3    9,48 19999999
SentinelLockQueue       Qno.  4   12,01 19999999
SentinelLockQueue       Qno.  5   14,13 19999999
SentinelLockQueue       Qno.  6   16,65 19999999
SentinelLockQueue       Qno.  7   18,99 19999999
Exception in thread "main" java.lang.OutOfMemoryError: Java heap space
*)
// As expected the computer runs out of memory. 
// running the original F# version (with noQueues = 60 and iterations = 600000):
(*
Qno.  0	     599999 00:00:00.1057046
Qno.  1	     599999 00:00:00.0758278
...
Qno. 49	     599999 00:00:00.0688158
Qno. 50	     599999 00:00:00.1296538
Qno. 51	     599999 00:00:00.0588410
Qno. 52	     599999 00:00:00.1296544
Qno. 53	     599999 00:00:00.0548537
Qno. 54	     599999 00:00:00.1306504
Qno. 55	     599999 00:00:00.0767949
Qno. 56	     599999 00:00:00.1146938
Qno. 57	     599999 00:00:00.1007287
Qno. 58	     599999 00:00:00.0718094
Qno. 59	     599999 00:00:00.1027257
Real: 00:00:05.971, CPU: 00:00:06.343, GC gen0: 219, gen1: 155, gen2: 32
val it : unit = ()
*)
// It is worth to note, that the F# implementation never runs out of memory on my computer, as the Java implementation does.

(*Exercise 2*)
// The problem is, that no garbage collection ever happens, since the get() method never removes any pointers, thereby keeping the
// entire linked list intact, even though the head is correctly moved down the list as we get() nodes. The fix for this is a simple line,
// where we make first.Next point to null(None), making it possible for the garbage collection to remove the nodes, that we already popped 
// (get()) from the queue. The F# implementation in this file has this correction. The fix for the Java version is identical.

(*Exercise 3*)
// running the fixed java code: 
(*
SentinelLockQueue       Qno.  0    0,54 19999999
SentinelLockQueue       Qno.  1    1,09 19999999
SentinelLockQueue       Qno.  2    1,67 19999999
SentinelLockQueue       Qno.  3    2,17 19999999
SentinelLockQueue       Qno.  4    2,67 19999999
SentinelLockQueue       Qno.  5    3,17 19999999
SentinelLockQueue       Qno.  6    3,66 19999999
SentinelLockQueue       Qno.  7    4,15 19999999
SentinelLockQueue       Qno.  8    4,65 19999999
SentinelLockQueue       Qno.  9    5,14 19999999
SentinelLockQueue       Qno. 10    5,63 19999999
SentinelLockQueue       Qno. 11    6,13 19999999
SentinelLockQueue       Qno. 12    6,62 19999999
SentinelLockQueue       Qno. 13    7,11 19999999
SentinelLockQueue       Qno. 14    7,61 19999999
SentinelLockQueue       Qno. 15    8,11 19999999
SentinelLockQueue       Qno. 16    8,61 19999999
SentinelLockQueue       Qno. 17    9,10 19999999
SentinelLockQueue       Qno. 18    9,60 19999999
SentinelLockQueue       Qno. 19   10,10 19999999
*)
// running the fixed F# version: 
(*
Qno.  0	     599999 00:00:00.0219405
Qno.  1	     599999 00:00:00.0189437
...
Qno. 49	     599999 00:00:00.0199435
Qno. 50	     599999 00:00:00.0189469
Qno. 51	     599999 00:00:00.0209461
Qno. 52	     599999 00:00:00.0199501
Qno. 53	     599999 00:00:00.0189447
Qno. 54	     599999 00:00:00.0219486
Qno. 55	     599999 00:00:00.0189432
Qno. 56	     599999 00:00:00.0189480
Qno. 57	     599999 00:00:00.0209432
Qno. 58	     599999 00:00:00.0189560
Qno. 59	     599999 00:00:00.0189480
Real: 00:00:01.222, CPU: 00:00:01.234, GC gen0: 321, gen1: 1, gen2: 1
val it : unit = ()
*)
// The fixed java version, does not run out of memory, and runs to completion. It also performs the tasks significantly faster.
// The fixed F# version achieves much faster running time. 