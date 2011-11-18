Student Name: Jonathan Neufeld
Student ID: 30671093
CS ID: p9d8

Problem 1

1) Continuations don't return. A function saves its data to the runtime stack before
calling another function, so when the callee is finished, control returns to the caller.
Continuations don't push data onto the stack, they take the data with it on the calls
to other functions. Thus, nothing is used on the stack.

2) Continuations can become unwieldy if the continuation becomes complex, e.g. from
significant memeory usage, or if the computation is so simple that adding a receiver
would increase overhead inefficiently. Tail-call optimisation may suffice without the
additional memory overhead in the latter case.

3) 
A: Evaluation of the current function might call other functions, so it's value can't
be predicted statically.
B: Values in the store can be accessed from any subsequent evaluations, even if they're
in a different branch of the AST.
C: Subsequent calls down the AST can create new bindings that "overwrite" old bindings.
D: The rest of the program, like threading a store through all branches of the AST.
E: Anything inside an expression and in the current environment is static scope.
F: Further calls and changing of values can occur before the initial continuation is
applied.
G: Everything in the AST, especially a subtree if the current node, can be predicted;
it's known statically.

4) 
R. interpreting: {{fun {f} {+ {f 0} 0}} {fun {ignore} deepness}} 
   in []

R.a interpreting: {fun {f} {+ {f 0} 0}}
    in [deepness -> 1]

R.b interpreting: {fun {ignore} deepness}
    in [deepness -> 1]

R.c interpreting: {+ {f 0} 0}
    in [deepness -> 1]
	   [f -> {fun {ignore} deepness}] 

R.c.a interpreting {f 0}
      in [deepness -> 1]
		 [f -> {fun {ignore} deepness}]
		 [deepness -> 2]

R.c.a.a interpreting f
        in [deepness -> 1]
		   [f -> {fun {ignore} deepness}]
		   [deepness -> 2]
		   [deepness -> 3]

R.c.a.b interpreting 0
        in [deepness -> 1]
		   [f -> {fun {ignore} deepness}]
		   [deepness -> 2]
		   [deepness -> 3]

R.c.a.c interpreting deepness
        in [deepness -> 1]
		   [f -> {fun {ignore} deepness}]
		   [deepness -> 2]
		   [deepness -> 3]

R.c.b interpreting 0
      in [deepness -> 1]
		 [f -> {fun {ignore} deepness}]
		 [deepness -> 2]

5) The new function will print "The value is true". Since the list is not empty,
we grab (first list) and apply k to it -- but k is the receiver given to the first
invocation of andall/k, the lambda function. We essentially "continue" right into
the initial continuation.

6) We call strict on a thunkV, so we need to interp the body of the thunkV. But the
body of the thunkV in this case in a numV. interp expects a CFWAE, but it's given a
CFWAE-Value, numV 0. This will result in an error.

7) In static scoping, this should result in an error since \foo isn't in the scope
of \cdbSemA, it's only defined within \cdbSemC. For the same reason, it can't be
dynamic scoping: even though \cdbSemC executed and \foo was defined, it was defined
in a subtree whose environment doesn't thread it's way back up the AST.

