-------------------------- MODULE MC3TwoPhasePA ------------------------
VARIABLES rmState, tmState, tmPrepared, msgs

INSTANCE TwoPhasePA WITH RM <- { "r1", "r2", "r3" }

========================================================================
