------------------------------ MODULE Test669 -------------------------------
EXTENDS Integers

CONSTANT
    \* @type: Int;
    N

VARIABLE
    \* @type: Int;
    x

Init == x = N
Next == x' = x - 1
=============================================================================
