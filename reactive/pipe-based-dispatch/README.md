Reactive dispatching based on pipe contents.  No blocking.
-------------------------------------------

In this form, we have the advantage that RECEIVE() *never* blocks.  The Dispatcher invokes a PID
only if it has input waiting for it in its pipe[?].in

In this form, SEND() *never* blocks, with the implication that queues are unbounded (see Q2 below).

See the diagram "reactive-pipe-based-dispatch.drawio".

On the diagram, a process is seen to consist of two states:

1. Initialization.
2. Steady-state.

In Lisp (and any language that can return a closure/continuation/callback) we can fold both states into one routine.
When called, the routine performs initialization and returns a closure to be used as the steady-state function.



Engineering Questions:


Q1: In general, the "stack" is unbounded.  How can an Engineer calculate that the stacks never overflow?

A1a: Perform dynamic stack-depth checking.
A1b: Guarantee that no steady-state routine (lambda) can call another steady-state routine.  Only the Dispatcher() can 
    call the steady-state routines.  Hence, the stack depth is one call-frame deep (Dispatcher->routine) plus the sum of
    all locals and all internal function calls within the steady-state routine.  


Q2: In this form, our queues are unbounded. How can an Engineer calculate that pipes never overflow memory?

A2: One possible answer:
- ensure that a process (PID) runs only once per dispatch cycle
- count the number of SEND()s in each process, sum the counts and that becomes the maximum depth of any (all) queues.
