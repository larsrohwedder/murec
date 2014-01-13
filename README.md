murec
=====
Interpreter for mu-recursive functions. 

Language: Haskell

Author: Lars Rohwedder

Requirements
-----
* ghc installed

Compiling
-----
    ghc Main.hs
 
Sample Useage
-----
File ''example.mu'':

    -- constants: C(number, arity)
    c0 := C(0, 0);
    -- compositions: N . (f1, f2 .. fn)
    c1 := N . c0;
    -- projection: P (index, arity)
    id := P(1, 1);
    -- primitive recursion  
    plus := R(id, N . P(1, 3));
    -- mu recursion: MU(f)
    z := MU(id);
    
Loading a definition file:

    ./Main example.mu
    
Evaluating an expression:
    
    > plus (1, 3)
    4
    > z ()
    0
    
Reloading definitions:

    > :r
    
Quitting:

    > :q
