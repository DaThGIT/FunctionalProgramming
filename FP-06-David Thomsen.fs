/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  6  ##############################
/// ###########################################################################
/// Author: Example Exampleson <example@itu.dk>
///
/// Notes:
/// - The anonymous let bindings below functions (the
///   `let _ = <some function name> : <some type>` statements) enforce type
///   constraints on functions given in the assignments (i.e. ensure that your
///   functions have the correct types) and are meant as a help to you.
///   If the function cannot be cast to the type specified in the assignment
///   text, a (type mismatch) error will be raised.
/// - The actual exercises start below the sections that have clearly been
///   marked as defining helper functions and type definitions.
module a6

(* Exercise 6.1 (HR 6.2) *)

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)
/// From Section 6.2 6.2 Symbolic differentiation
type Fexpr = | Const of float
                | X
                | Add of Fexpr * Fexpr
                | Sub of Fexpr * Fexpr
                | Mul of Fexpr * Fexpr
                | Div of Fexpr * Fexpr
                | Sin of Fexpr
                | Cos of Fexpr
                | Log of Fexpr
                | Exp of Fexpr

/// Given a (float) value of the variable, `x`, compute the float value of
/// a given `Fexpr`
let rec compute x = function
    | Const r      -> r
    | X            -> x
    | Add(fe1,fe2) -> compute x fe1 + compute x fe2
    | Sub(fe1,fe2) -> compute x fe1 - compute x fe2
    | Mul(fe1,fe2) -> compute x fe1 * compute x fe2
    | Div(fe1,fe2) -> compute x fe1 / compute x fe2
    | Sin fe       -> System.Math.Sin (compute x fe)
    | Cos fe       -> System.Math.Cos (compute x fe)
    | Log fe       -> System.Math.Log (compute x fe)
    | Exp fe       -> System.Math.Exp (compute x fe)
let _ = compute : float -> Fexpr -> float // constrained to float because Const has a float

/// Differentiate the given `Fexpr`
let rec D = function
    | Const _    -> Const 0.0
    | X          -> Const 1.0
    | Add(fe,ge) -> Add(D fe, D ge)
    | Sub(fe,ge) -> Sub(D fe, D ge)
    | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
    | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)),
                        Mul(ge,ge))
    | Sin fe     -> Mul(Cos fe, D fe)
    | Cos fe     -> Mul(Const -1.0, Mul(Sin fe, D fe))
    | Log fe     -> Div(D fe, fe)
    | Exp fe     -> Mul(Exp fe, D fe)
let _ = D : Fexpr -> Fexpr

/// Generate a "simple" textual representation of the given `Fexpr` (simple
/// in the sense that it contains all parentheses)
let rec toString = function
    | Const x       -> string x
    | X             -> "x"
    | Add(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " + " + "(" + (toString fe2) + ")"
    | Sub(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " - " + "(" + (toString fe2) + ")"
    | Mul(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " * " + "(" + (toString fe2) + ")"
    | Div(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " / " + "(" + (toString fe2) + ")"
    | Sin fe        -> "(sin " + (toString fe) + ")"
    | Cos fe        -> "(cos " + (toString fe) + ")"
    | Log fe        -> "(log " + (toString fe) + ")"
    | Exp fe        -> "(exp " + (toString fe) + ")"
let _ = toString : Fexpr -> string

/// Extend `Fexpr` with some convenient instance methods to showcase this
/// language facility, which is used in 6.3 (HR 7.2).
type Fexpr with
    member this.Compute x = compute x this
    member this.Differentiate = D this
    override this.ToString() = toString this

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


/// Generate a postfix string representation of the given Fexpr
let rec toPostfixString e = failwith "Not implemented"
let _ = toPostfixString : Fexpr -> string


(* Exercise 6.2 (HR 6.8) *)

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)
type Instruction = ADD
                    | SUB
                    | MULT
                    | DIV
                    | SIN
                    | COS
                    | LOG
                    | EXP
                    | PUSH of float

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)

(* Exercise 6.2.1 (HR 6.8.1) *)

/// A type representing the stack
type Stack = string // <-- NOTE TODO FIXME this implementation is WRONG. Please define correct type for exercise.

/// Interpret the execution of a single instruction on the given Stack
let intpInstr (s :Stack) (i :Instruction) = failwith "Not implemented"
let _ = intpInstr : Stack -> Instruction -> Stack

(* Exercise 6.2.2 (HR 6.8.2) *)

/// Interpret the execution of a programme (which is defined as a list of
/// `Instruction`s as per the exercise text)
let intpProg is = failwith "Not implemented"
let _ = intpProg : Instruction list -> float


(* Exercise 6.2.3 (HR 6.8.3) *)

/// Transform an `Fexpr` to a programme executable by intpProg, by substituting
/// all occurrences of the variable, `x`, with the given floating point numeric
/// value.
let trans (fe,x) = failwith "Not implemented"
let _ = trans : Fexpr * float -> Instruction list


(* Exercise 6.3 (HR 7.2) *)

/// Since the assignment text explicitly says to only upload one file, the
/// signature-file's content is outlined below:
///
///
/// //Filename: CN.fs
///
/// module CN
/// [<Sealed>]
/// type Complex =
///     static member ( + ) : TODO FOR EXERCISE 6.3 place type annotation here
///     static member ( - ) : TODO FOR EXERCISE 6.3 place type annotation here
///     static member ( * ) : TODO FOR EXERCISE 6.3 place type annotation here
///     static member ( / ) : TODO FOR EXERCISE 6.3 place type annotation here
///
///
/// This signature file should have a corresponding implementation file CN.fs
/// with the following contents (the `module` declaration at the top should be
/// commented in):

//module CN
type Complex = C of float * float with
    /// Get the sum of two complex numbers
    static member ( + ) (C (r1,c1), C (r2,c2)) = failwith "Not implemented"
    /// Get the product of two complex numbers
    static member ( * ) (C (r1,c1), C (r2,c2)) = failwith "Not implemented"
    /// Get the difference between two complex numbers
    static member ( - ) (C (r1,c1), C(r2,c2)) = failwith "Not implemented"
    /// Get the quotient between two complex numbers without recalculating the
    /// squared sum of divisors in resulting tuple
    static member ( / ) (C (r1,c1), C (r2,c2)) = failwith "Not implemented"

