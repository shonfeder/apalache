----- MODULE Assignments20200309 -----
VARIABLE
    \* @type: Int;
    a

\* this specification fails, as it has no expression
\* that can be treated as an assignment
Init == TRUE
Next == a' = a
Inv == FALSE
===============
