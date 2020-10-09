# Apalache CLI Integration Tests

The code blocks in this file use [mdx](https://github.com/realworldocaml/mdx) to
run integration tests of the Apalache CLI interface.

To run these tests, execute the [../mdx-test.py](../mdx-test.py) script with no
arguments.

## How to write a test

Any `sh` code block will be run as a test.

The test executes the command following a `$`. The command is expected to
produce the output on the subsequent lines of the code block.

Some tips:

- Use `...` to omit irrelevant lines in output. This is useful for
  nondeterministic output or to hide noise.
- Specify a non-zero return code `n` by adding `[n]` on a line by itself after
  the output.
- Pipe into filters to get arbitrary control of the expected output.

The usual flow is:

1. Write a failing test that executes the command to be run.
2. Run the test (see below).
3. Check that the corrected output is what you expect, then run `make promote`,
   to copy the output back into this file.
4. Replace any non-essential lines with `...`.

See the documentation on `mdx` for more features and flexibility.

## How to run tests

(From the project root.)

### Run all the tests in this file

<!-- $MDX skip -->
```sh
test/mdx-test.py
```

### Run a single test

Each section, demarcated by headings, can be run selectively by supplying an
argument that matches the heading.

E.g., to run just the test for the `version` command, run

<!-- $MDX skip -->
```sh
test/mdx-test.py "executable prints version"
```

**NOTE**: This only runs code blocks directly in the named section, and will not
include execution of blocks in nested subsections.

### Run all tests in sections matching a pattern

The matching is based on (perl) pattern matching. E.g., you can run all the
tests in sections that include the string `"executable"` in their headings with

<!-- $MDX skip -->
```sh
test/mdx-test.py executable
```

## test environment

### working directory

```sh
$ pwd | grep -o test/tla
test/tla
```

## executing the binary

### executable prints version

```sh
$ apalache-mc version
...
EXITCODE: OK
```

### executable prints help

```sh
$ apalache-mc help
...
EXITCODE: OK
```

## running the check command

### incremental check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=incremental --length=1 --init=Init --next=Next --inv=Inv Bug20190118.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=offline --length=1 --init=Init --next=Next --inv=Inv Bug20190118.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=1 --init=Init --next=Next --inv=Inv Bug20190118.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check mis.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=IsIndependent mis.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check mis.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --inv=IsIndependent mis.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check mis.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=IsIndependent mis.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check mis_bug.tla errors

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=IsIndependent mis_bug.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### offline check mis_bug.tla errors

```sh
$ apalache-mc check --algo=offline --length=5 --inv=IsIndependent mis_bug.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### parallel check mis_bug.tla errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=IsIndependent mis_bug.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
Checker has found an error
...
```

### incremental check ast.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 ast.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check ast.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 ast.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check ast.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 ast.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check pr.tla suceeds

```sh
$ apalache-mc check --algo=incremental --length=2 pr.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check pr.tla suceeds

```sh
$ apalache-mc check --algo=offline --length=2 pr.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check pr.tla suceeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=2 pr.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check EWD840.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=Inv EWD840.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check EWD840.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --inv=Inv EWD840.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check EWD840.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=Inv EWD840.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Paxos.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=Inv Paxos.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Paxos.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --inv=Inv Paxos.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Paxos.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=Inv Paxos.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=incremental --length=1 Bug20190118.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=offline --length=1 Bug20190118.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Bug20190118 succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=1 Bug20190118.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Bug20190921 succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --cinit=CInit Bug20190921.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Bug20190921 succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --cinit=CInit Bug20190921.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Bug20190921 succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --cinit=CInit Bug20190921.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Counter.tla errors

```sh
$ apalache-mc check --algo=incremental --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### offline check Counter.tla errors

```sh
$ apalache-mc check --algo=offline --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### parallel check Counter.tla errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
Checker has found an error
...
```

### y2k.tla

#### incremental check y2k with length 20 and ConstInit errors

```sh
$ apalache-mc check --algo=incremental --length=20 --inv=Safety --cinit=ConstInit y2k_cinit.tla  | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

#### offline check y2k with length 20 and ConstInit errors

```sh
$ apalache-mc check --algo=offline --length=20 --inv=Safety --cinit=ConstInit y2k_cinit.tla  | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

#### parallel check y2k with length 20 and ConstInit errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=20 --inv=Safety --cinit=ConstInit y2k_cinit.tla  | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
Checker has found an error
...
```

#### incremental check y2k with 19 steps succeeds

```sh
$ apalache-mc check --algo=incremental --length=19 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

#### offline check y2k with 19 steps succeeds

```sh
$ apalache-mc check --algo=offline --length=19 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

#### parallel check y2k with 19 steps succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=19 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

#### incremental check y2k with length 30 errors

```sh
$ apalache-mc check --algo=incremental --length=30 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

#### offline check y2k with length 30 errors

```sh
$ apalache-mc check --algo=offline --length=30 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

#### parallel check y2k with length 30 errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=30 --inv=Safety y2k_instance.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
Checker has found an error
...
```

### incremental check Counter.tla errors

```sh
$ apalache-mc check --algo=incremental --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### offline check Counter.tla errors

```sh
$ apalache-mc check --algo=offline --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
The outcome is: Error
Checker has found an error
...
```

### parallel check Counter.tla errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --inv=Inv Counter.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
Checker has found an error
...
```

### incremental check NatCounter.tla errors

```sh
$ apalache-mc check --algo=incremental --length=10 --inv=Inv NatCounter.tla  | sed 's/I@.*//'
...
The outcome is: Error
...
```

### offline check NatCounter.tla errors

```sh
$ apalache-mc check --algo=offline --length=10 --inv=Inv NatCounter.tla  | sed 's/I@.*//'
...
The outcome is: Error
...
```

### parallel check NatCounter.tla errors

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --inv=Inv NatCounter.tla  | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
...
```

### incremental check NeedForTypesWithTypes.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=10 --cinit=ConstInit --inv=Inv NeedForTypesWithTypes.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check NeedForTypesWithTypes.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=10 --cinit=ConstInit --inv=Inv NeedForTypesWithTypes.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check NeedForTypesWithTypes.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --cinit=ConstInit --inv=Inv NeedForTypesWithTypes.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check HandshakeWithTypes.tla with length 4 succeeds

```sh
$ apalache-mc check --algo=incremental --length=4 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check HandshakeWithTypes.tla with length 4 succeeds

```sh
$ apalache-mc check --algo=offline --length=4 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check HandshakeWithTypes.tla with length 4 succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=4 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check HandshakeWithTypes.tla with lengh 5 deadlocks

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
The outcome is: Deadlock
...
```

### offline check HandshakeWithTypes.tla with lengh 5 deadlocks

```sh
$ apalache-mc check --algo=offline --length=5 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
The outcome is: Deadlock
...
```

### parallel check HandshakeWithTypes.tla with lengh 5 deadlocks

The parallel checker does not find deadlocks yet

$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=Inv HandshakeWithTypes.tla | sed 's/I@.*//'
...
The outcome is: Deadlock
...

### incremental check trivial violation of FALSE invariant

```sh
$ apalache-mc check --algo=incremental --length=2 --inv=Inv Bug20200306.tla | sed 's/I@.*//'
...
The outcome is: Error
...
```

### offline check trivial violation of FALSE invariant

```sh
$ apalache-mc check --algo=offline --length=2 --inv=Inv Bug20200306.tla | sed 's/I@.*//'
...
The outcome is: Error
...
```

### parallel check trivial violation of FALSE invariant

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=2 --inv=Inv Bug20200306.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: Error
...
```

### incremental check Init without an assignment fails

```sh
$ apalache-mc check --algo=incremental --length=1 --inv=Inv Assignments20200309.tla
...
EXITCODE: ERROR (99)
[99]
```

### offline check Init without an assignment fails

```sh
$ apalache-mc check --algo=offline --length=1 --inv=Inv Assignments20200309.tla
...
EXITCODE: ERROR (99)
[99]
```

### parallel check Init without an assignment fails

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=1 --inv=Inv Assignments20200309.tla
...
EXITCODE: ERROR (99)
[99]
```

### incremental check Inline.tla suceeds

```sh
$ apalache-mc check --algo=incremental --length=5 Inline.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Inline.tla suceeds

```sh
$ apalache-mc check --algo=offline --length=5 Inline.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Inline.tla suceeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 Inline.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Rec1.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=Inv Rec1.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Rec1.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --inv=Inv Rec1.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Rec1.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=Inv Rec1.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Rec3.tla succeeds
```sh
$ apalache-mc check --algo=incremental --length=10 --inv=Inv Rec3.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Rec3.tla succeeds
```sh
$ apalache-mc check --algo=offline --length=10 --inv=Inv Rec3.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```
### parallel check Rec3.tla succeeds
```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --inv=Inv Rec3.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Rec8.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=10 --inv=Inv Rec8.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Rec8.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=10 --inv=Inv Rec8.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Rec8.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=10 --inv=Inv Rec8.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Rec9.tla succeeds

```sh
$ apalache-mc check --algo=incremental --length=5 --inv=Inv Rec9.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check Rec9.tla succeeds

```sh
$ apalache-mc check --algo=offline --length=5 --inv=Inv Rec9.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check Rec9.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --length=5 --inv=Inv Rec9.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check ExistsAsValue.tla succeeds

```sh
$ apalache-mc check --algo=incremental --inv=Inv ExistsAsValue.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check ExistsAsValue.tla succeeds

```sh
$ apalache-mc check --algo=offline --inv=Inv ExistsAsValue.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check ExistsAsValue.tla succeeds

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --inv=Inv ExistsAsValue.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

### incremental check Empty.tla fails

```sh
$ apalache-mc check --algo=incremental Empty.tla
...
EXITCODE: ERROR (99)
[99]
```

### offline check Empty.tla fails

```sh
$ apalache-mc check --algo=offline Empty.tla
...
EXITCODE: ERROR (99)
[99]
```

### parallel check Empty.tla fails

```sh
$ apalache-mc check --algo=parallel --nworkers=1 Empty.tla
...
EXITCODE: ERROR (99)
[99]
```

### incremental check HourClock.tla without Init fails

```sh
$ apalache-mc check --algo=incremental --init=NonExistantInit HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### offline check HourClock.tla without Init fails

```sh
$ apalache-mc check --algo=offline --init=NonExistantInit HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### parallel check HourClock.tla without Init fails

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --init=NonExistantInit HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### incremental check HourClock.tla without Next fails

```sh
$ apalache-mc check --algo=incremental --next=NonExistantNext HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### offline check HourClock.tla without Next fails

```sh
$ apalache-mc check --algo=offline --next=NonExistantNext HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### parallel check HourClock.tla without Next fails

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --next=NonExistantNext HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### incremental check HourClock.tla without Inv fails

```sh
$ apalache-mc check --algo=incremental --inv=NonExistantInv HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### offline check HourClock.tla without Inv fails

```sh
$ apalache-mc check --algo=offline --inv=NonExistantInv HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### parallel check HourClock.tla without Inv fails

```sh
$ apalache-mc check --algo=parallel --nworkers=1 --inv=NonExistantInv HourClock.tla
...
EXITCODE: ERROR (99)
[99]
```

### incremental check use of TLA_PATH for modules in child directory succeeds

```sh
$ TLA_PATH=./tla-path-tests apalache-mc check --algo=incremental ./tla-path-tests/ImportingModule.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### offline check use of TLA_PATH for modules in child directory succeeds

```sh
$ TLA_PATH=./tla-path-tests apalache-mc check --algo=offline ./tla-path-tests/ImportingModule.tla | sed 's/I@.*//'
...
The outcome is: NoError
...
```

### parallel check use of TLA_PATH for modules in child directory succeeds

```sh
$ TLA_PATH=./tla-path-tests apalache-mc check --algo=parallel --nworkers=1 ./tla-path-tests/ImportingModule.tla | sed 's/I@.*//'
...
Worker 1: The outcome is: NoError
...
```

