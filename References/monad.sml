(* Ben's Brief Introduction to Monads

Caveat: this is NOT a topic that I know cold, so this will be a little
rough.  But here's how I think about it.

 *)
val SOME max = Int.maxInt
val SOME min = Int.minInt
                     

(* Let's start by motivating a Checked monad.  (You may have seen the
Maybe monad elsewhere -- basically same.)

Suppose we want to use this overflow-checked add function.  It
explicitly checks for overflow and returns NONE if overflow occurs,
otherwise it reutrns SOME of the answer. By using this option return
type, it forces the client to account for errors in their
computations, much as with checked exceptions in a language like Java.
(i.e., errors do not go by silently) To write computations using
add_checked, we have to do pattern-match everywhere we need a result
from add_checked. *)
                   
fun add_checked (x,y) =
    if (x >= 0 andalso (max - x) > y)
       orelse (x < 0 andalso (x - min) > y)
    then SOME (x+y)
    else NONE

val foo1 = case add_checked (~2,4) of
               NONE => NONE
             | SOME z => (case add_checked (5, z) of
                              NONE => NONE
                            | SOME a => 
                              if a > 7
                              then SOME a
                              else SOME 7)
(* It's easy to lose sight of the actual computation amid all the
NONE-SOME checking.  We could push the checking into the add_checked
implementation, but that has its own problems: do we control that
implementation?  Now we have to wrap up arguments in SOME... etc.  *)
fun add_checked2 (SOME x,SOME y) =
    if (x >= 0 andalso (max - x) > y)
       orelse (x < 0 andalso (x - min) > y)
    then SOME (x+y)
    else NONE
  | add_checked2 _ = NONE

val foo2 = case add_checked2(SOME 5,add_checked2(SOME ~2,SOME 4)) of
               NONE => NONE
             | SOME a => if a > 7
                         then SOME a
                         else SOME 7

(* More problems: this implementation always makes all the calls to
add_checked, even if the first call (inner-most call) results in an
error.

Both implementations force either the library-writer (implementer of
add_checked) or the client (user of add_checked) to reason about both
the logic of their program *and* the threading-through of error-ness
at the same time in the same code.  These concerns are should be
separate, but they are tangled up.

Another approach would be to use an exception instead of an option:
*)
fun add_checked3 (x,y) =
    if (x >= 0 andalso (max - x) > y)
       orelse (x < 0 andalso (x - min) > y)
    then x+y
    else raise Overflow
val foo3 = SOME let val a = add_checked3 (5, add_checked3 (~2,4))
                in if a > 7
                   then a
                   else 7
                end
           handle Overflow => NONE
(* This large separate the concerns of error-threading and logic.  If
the first (inner) add fails, control returns straight to the exception
handler -- the second add_checked3 is not called.  The client code
need only reason about errors where they really matter; the library
code need only reason about errors where they occur.

However, exceptions are problematic in a lazy language.  And even in
an eager language, they are not necessarily the right choice for the
"unexpected" branch of behavior.  (Consider looking up the value for a
key in an association list.)
 *)

(* Let's try one more: Continutation-Passing Style, where we never use
the return value of a function.  Instead, we pass an extra argument
which is the function to call on the result to continue computing once
this function is done.  We will pass one more argument: a function to
call to continue the computation if an error occurs.

Here is add (and our use of it) in continuation-passing style.
 *)

fun add_checked4 (x,y,cont,err) =
    if (x >= 0 andalso (max - x) > y)
       orelse (x < 0 andalso (x - min) > y)
    then cont (x+y)
    else err ()
    
val foo4 = add_checked4 (~2,4,
                         fn z => add_checked4 (5, z,
                                                fn a =>
                                                    if a > 7
                                                    then SOME a
                                                    else SOME 7,
                                                fn () => NONE),
                         fn () => NONE)
(* This works too, and avoids the second add operation if the first
fails, but it is also not very direct: it still forces the user to
tangle what to do and how to react to errors.  (Interesting side
notes:
  - It is always possible to convert a direct-style program into a CPS
    program.  In fact, this is how compilation of languages like ML
    typically works.)
  - CPS and the idea of continuations are more general than what is 
    shown here!


Enter monads, which may help solve our problem.
*)


(*
A monad is a module with these three things:
  - a monad type: a box for this kind of monad.
  - a return function: takes a thing and puts it in a box.
  - a bind function takes:
      - a box (containing a thing); and
      - a transformation to do to that thing to produce another box of
        the same kind containing a thing (potentially of a different
        type than the first thing) box.
    and produces:
      another of the same kind of box holding that transformed thing.
  - typically: some set of associated operations.

A better name for "box" is "computational context".  More later.
*)
signature MONAD = sig
    type 'a monad
    val return : 'a -> 'a monad
    val bind   : 'a monad -> ('a -> 'b monad) -> 'b monad
end

(*

The Checked monad does overflow-checked arithmetic and emulates an
Overflow exception without actually throwing an exception.  It does
this through the bind function, which lets us chain together several
potentially-failing computations that we want to do in order -- but
only if all the previous computations succeeded.  It does this by
calling the "next thing to do" function on the "result of the last
thing" only if the last thing succeeded.  If the result of the last
thing is an error, then bind immediately returns that Error rather
than trying to run the "next thing."

We use OK and Error here, but we could use an option instead.

We create an add function that takes two ints, does overflow-checking
and returns a Checked.monad.  It returns the sum wrapped in OK if no
overflow occurs.  If overflow occurs, it returns Error.

The return function simply wraps a value (evidence of a successful
computation) in an OK.

*)
signature CHECKED = sig
    include MONAD
    val add : int * int -> int monad
end
structure Checked :> CHECKED = struct
  datatype 'a monad = OK of 'a | Error
  fun return x = OK x
  fun bind (OK x) k = k x
    | bind Error k = Error

  fun add (x,y) : int monad =
      if (x >= 0 andalso (max - x) > y)
         orelse (x < 0 andalso (x - min) > y)
      then OK (x+y) (* or: return (x+y) *)
      else Error
end

(* Use names Checked.x as x. *)
open Checked
                          
(* Now we can use bind to sequence computations, with the potential
for early termination due to Error.  This is accomplished by calling
functions that produce results wrapped in the Checked.monad type, then
using bind to call a second function which is "the rest of what we
want to do after getting this result".  The latter function can be
called a _continuation_.

A related style of functional programming, Continuation-Passing Style,
uses a similar idea: never use the return value of a function.
Instead, every function should accept an extra argument which is
another function that should be called on the result of this function.
Instead of returning a value, this _continuation_ function is called
on the "return value".  The monadic style used by the Checked module
essentially interjects at this continuation call to provide a bit more
structure and automatically check for errors.
*)
val foo =
    (* We first add -2 and 4.  The result is either OK of a value or
    it is an Error.  In the former case, bind calls the continuation
    function on the unwrapped result.  In the latter case, bind
    immediately yields Error, never calling the continuation (i.e.,
    terminating early.)
    *)
    bind (add (~2,4))
         (fn z =>
             (* If the previous call to add succeeded, z is now bound
             to its unwrapped result.  Add 5 to z and do the
             continuation if it does not result in error.*)
             (bind (add (5, z))
                   (fn a =>
                       (* If the previous call to add succeeded, a is
                       now bound to its unwrapped result.  If the
                       result is greater than 7, "return" the result,
                       otherwise "return" 7. This "return" terminates
                       the sequencing with bind and yields a box hold
                       a or 7.  This is the final result bound to
                       foo. *)
                       if a > 7
                       then return a
                       else return 7)))
(* What happens if we replace by min? *)

(*
Now if we reformat just a bit and read right to left, we can see
imperative-looking code.  Haskell has special syntax for this, shown
in comments.
*)
val foo_reformat = (* do *)
    (* z <- add(~2,4) *)
    bind (add (~2,4)) (fn z =>
    (* a <- add(4,z) *)
   (bind (add (4, z)) (fn a =>
    if a > 7
    then return a
    else return 7)))

(*
There is still syntactic noise here, but:
  - Haskell removes it with syntactic sugar for this pattern.
  - Neither the client nor the library writer had to tangle
    error-threading and application logic in one!
  - There is no new language feature required to do this!


The purpose of the Checked monad was to thread the error status of the
computation through multiple stages WITHOUT DEALING WITH ERROR RESULTS
EXPLICITLY in any of those stages.  In other words, add never checks
its inputs to see whether they are valid.  We define add only in terms
of valid inputs.

This implements a clean separation of concerns:
  - the Checked monad implementation deals with:
    - early termination due to errors
    - passing non-error results on to the next stage of computation
  - the add function implements add, but only needs to do so over
    valid inputs
  - the client code composes the operations of this monad (just one in
    this case: add) without worrying at all about error-checking.

In other words, the monad packages operations and a way to manage
their computational context.  The benefits are multiple:
  - The client does not need to manage context explicitly.
  - The client CANNOT manage context explicitly.
  - The client can use operations of the monad ONLY WITHIN THE CONTEXT
    MANAGED BY THE MONAD.
  - The only way for the client to use results computed within this
    context, is to run more computations in the same context, under
    the control of the monad, naturally.
  - The client cannot accidentally use the monad operations without
    reasoning about the effects that can occur.

*)


(* Refless Refs.

Let's try another monad, the State monad.  This one will let us
emulate mutable state without actual mutation!

It is possible to do this explicitly without this monad: EVERY
potentially state-updating function in your program must take an extra
argument (the current state of the world) and return an extra value
(the new state of the world).  To "mutate" the state, a function
simply produces a new state as a return value, without actually
mutating the given state.  All functions that call state-mutating
functions, must take this return value and use it for all future
state-dependent functions.  This works, but beyond some simple
patterns, it's awkward.  Which functions might update state?  How do
you ensure you "passed on" the new state appropriately? etc.


Using the monad provides a means of separating concerns:
  - what do we want to do to the state?
  - how is updated state "passed on" to subsequent code?
It also forces the client to deal with the state changes properly --
there is no way to use the result except to do another stateful
operation.

Without further ado:
*)
signature STATE = sig
    (* Think of 'r as the result type and 's as the state type. *)
    type ('r,'s) monad
    (* return takes a result and produces that result and the new
    state. *)
    val return : 'r -> ('r,'s) monad
    (* bind takes a result-and-state and a way to take that result and
    produce a new result-and-state, then it produces a new
    result-and-sate *)
    val bind   : ('r,'s) monad -> ('r -> ('t,'s) monad) -> ('t,'s) monad

    (* get the current state: produces a state-as-result-and-state *)
    val get : ('s,'s) monad
    (* set the current state: takes a new state and produces a
    unit-as-result-and-new-state *)
    val put : 's -> (unit, 's) monad
    (* apply the given transformation to the current state *)
    val modify : ('s -> 's) -> (unit, 's) monad
    (* apply to initial state *)
    val run : ('r, 's) monad -> 's -> 'r * 's
end
                      
structure State :> STATE = struct
    (* The state monad works with state-processor functions:
       things that, given a state, produce a result and new state. *)
    type ('r,'s) monad = 's -> 'r * 's

    fun return result =
        fn state => (result, state)
    fun bind transform continuation =
        fn state =>
           let val (result, newstate) = transform state
           in continuation result newstate end

    (* get is a transformer that takes the current state
       and produces that state as both result and new state *)
    val get = fn state => (state,state)
    (* set takes a new state and creates a transformer that takes
       the old state and produces () as result and the given 
       new state.  (It discards the current/old state.) *)
    fun put newstate = fn oldstate => ((),newstate)
    (* modify takes a state-modification function and creates a
       transformer that takes the old state and produces result ()
       and the new state obtained by applying the modification
       to the old state. *)
    fun modify modification =
        fn oldstate => ((), modification oldstate)
    (* run applies a transformer to a state *)
    fun run transform state = transform state
end

                               
(*
Let's use it to store an implicit "mutable" counter.
*)
open State
(* inc is a state transformer that tranforms the counter by
incrementing it. *)
val inc = modify (fn x => x + 1)
val (endstate,_) = run
                     (bind inc (fn _ =>
                      bind inc (fn _ =>
                      bind inc (fn _ =>
                      bind get return))))
                     0

(*
This could be read as (pseudo-code):

val endstate = do
  := 0;
  ++;
  ++;
  ++;
  return

Notice that the original code never actually names "the state
variable".  It is threaded through by the monad machinery, so each
monad operation refers to it implicitly.  Giving it a name makes the
pseudocode clearer to those familiar with imperative languages:

val endstate = do
  counter := 0;
  counter++;
  counter++;
  counter++;
  return counter

One thing comes out of order: the initial state appears as the 2nd
argument to run, after the "what to do".  We could flip the arguments
to run to make it appear first in order.

This example serves to illustrate another important point: the first
argument to the run function (the big (bind ...)) is just that -- an
argument to the run function.  It is a VALUE.  It does happen to be a
function that is a value, but it is a value.  This means that
compuation and state-updating does not actually happen as the bind
lines are evaluated.  Instead they build up a description of an
action.  The run function then takes that action and applies it to the
initial state to produce the final state.

For a lazy language, this would be very useful for managing state.
Operations that update state directly through mutation may evaluate in
arbitrary order due to laziness.  This makes it very difficult to
reason about the order in which state updates will occur.  Simply
writing the updates in lexical order does not necessarily suffice.  We
have instead constructed an _action_ that describes a sequence of
state transformations that should occur.  By making each stage in the
sequence of state-reliant computations depend explicitly on the result
of the previous stage, any execution (lazy or not) is forced to
respect these dependencies.  The stages of the action are guaranteed
to run in order.

The state monad defined here provides the run function, which allows
us to run a sequence of stateful commands from an initial state and
"get the results out" to use in pure non-monadic code.  It is fine
(and easy) for monadic state-carrying code to call into pure code.
The important restriction going forward is that pure code can call
stateful code _only_ by using the run function.

Another way to say all this might be that using the monadic style
allows to treat effects as first-class values: things we can pass
around (or discard) explcitly.

The same tool can also be used to sequester true side effects.
Consider the IO monad.  Unlike the State monad, it provides no escape
hatch: there is no wait to get results out of the monad and use them
in pure code.  Huit clos.  (Code in the monad can call pure code and
use its results, but not vice versa.)  If we carefully define all
side-effectful operations to wrap their results in the IO monad, we
can keep their sequencing entirely under our control, even in a lazy
language.  Like the State monad, side-effectful operations do not
actually do side effects immediately: their return values are actions
to be executed later.  Unlike the State monad, the only way to "get"
the result of an IO action (a.k.a., force the IO action to complete)
is to put it as the main entry point of the program.  (Here, we're emulating this in SML -- Haskell actually sets up main 

*)

signature IO = sig
    include MONAD
    val main : unit monad -> unit
    (* Now, each mechanism requiring side effects must simply wrap its
       return values in the monad to make them impossible to extract
       except via bind.  Let us take mutable references as an example.
       Since we can't go back and change the implementation of refs,
       we'll wrap them up in a new module and not use refs
       directly. *)

    val print : string -> unit monad

    structure Ref : sig
        (* Create a fresh reference, initially holding the given
           value. *)
        val new : 'a -> 'a ref monad
        (* Get the current contents of a reference cell. *)
        val get : 'a ref -> 'a monad
        (* Set the contents of a reference cell to the given value. *)
        val put : 'a ref -> 'a -> unit monad
    end
end                                                                  

(* Notice that the shape is very similar to that of the State monad.
Here we are implicitly threading the state of the world through our
monadic computations. *)

structure IO :> IO = struct
    datatype world = World
    type 'a monad = world -> 'a * world
    fun return x = fn w => (x,w)
    fun bind x f = (fn w => let val (result, w') = x w
                            in f result w' end)
    exception OnlyOnce
    val main =
        let val onlyOnce = ref true
        in
            fn m => if (!onlyOnce)
                    then let val ((),World) = m World
                         in onlyOnce := false end
                    else raise OnlyOnce
        end

    val print = fn s => fn w => (print s,w)

    structure Ref = struct
        fun new x = fn w => (ref x, w)
        fun get r = fn w => (!r, w)
        fun put r x = fn w => (r := x, w)
    end
end

(* This won't work
val r = Ref.new 0
val a = Ref.get r *)
open IO
val () =
  IO.main (
    bind (Ref.new 0) (fn r =>
    bind (Ref.get r) (fn x =>
    bind (Ref.put r (x + 1)) (fn () =>
    bind (Ref.get r) (fn y =>
    if y > 0
    then (IO.print ("Incrememented: " ^ (Int.toString y) ^ "\n"))
    else (IO.print ("Not incremented: " ^ (Int.toString y) ^ "\n"))
    ))))
  ) 
