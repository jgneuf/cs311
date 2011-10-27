Student Name: Jonathan Neufeld
Student ID: 30671093
CS ID: p9d8

1) The first evaluation of primes !! 1000 takes much longer than the second. The second
is much quicker because (probably) Hugs/Haskell implements caching; it evaluated the
expression primes !! 1000, cached the result and simply does a lookup the second time.

2) isPrime 104729 ran too quickly, so I tried isPrime 10472917. Both the first and 
second evaluations took some time, so there was no caching. The third evaluation also
took some time. primes is evaluated lazily by necessity, but isPrime is using eager
evaluation.

3) A lazy interpreter would come up with different results under static scoping than it
would under dynamic scoping.

{with {x 5}
    {with {y {+ x x}}
        {with {x 10}
            {+ x y}}}}

Static: x -> 10 from most recent binding, y -> 10, becuase the named expression {+ x x}
is {+ 5 5}. The result is 20.
Dynamic: x -> 10 as before, y -> {+ x x} which evaluates to {+ 10 10}. Now the result
is 30.

4) 
{with {x {read-eval}}
    x}
In this case there's obviously no way to predict the value of x, since it depends on
user input, i.e. x's value is created dynamically. 

{with {complex-computation {fun {n} ...}}
    {with {x {complex-computation 10}}
        x}}
Consider the second with. When it is being evaluated, we know x will be bound to the
result of {complex-computation 10}, but we have no idea what that will evaluate to,
since it's very complex! That is, after all, why we're letting our program do all the
work.

{rec {fact {fun {n} {if0 n 1 {* n {fact {- n 1}}}}}}
    {fact 10}}
It's not clear what fact is bound to in this case, especially since it's a recursive
definition. We had enough trouble with complex-computation in the previous piece of
code, and this is similar. Since fact calls itself, however, we have no idea what n
is bound to at any point in the program statically, since (in this case) it could be
[0..10].

5) 
{with {countdown {fun {n} {if0 n 0 {countdown {- n 1}}}}}
  {countdown 0}}
Static: 0.
Dynamic: 0.

{+ {with {countdown {fun {n} {if0 n 0 {countdown {- n 1}}}}}
     {countdown 3}}
   {countdown 0}}
Static: Error unbound identifier countdown in the else case of if0 when 3 is applied to
countdown. In that environment, countdown is not bound.
Dynamic: Error unbound identifier countdown in the rhs of the binop. The rhs of binop
binds a function to countdown and, under dynamic scoping, {countdown 3} evaluates to
0. But the closureV for countdown does not exist outside of the with it is created in,
so {countdown 0} results in an error.

6) Haskell will be as lazy as it can in this case, but it still has to evaluate every
element in the list [1..10000000]. filter applies allones to each element of the given
list, which in this case is [1..10000000] after having each of its elements squared
then subtracting one. So Haskell takes the first element of the mapped expression and
calls allones with it, then takes the next element of the mapped expression, etc. But
to evaluate result, Haskell still has to pull each element from the mapped expression.

7) Using this piece of code:
{with* {{x 2} {y 1}} {+ {* x x} y}}

First consider what the preprocessor from assignment 1 does. We convert the with* to
a series of nested withs according to this rule:
{with* {{<id1> <ne1>} <rest-of-bindings>} <body>} =>
    {with {<id1> <ne1>} {with* {<rest-of-bindings>} <body>}}

where id1 = x, ne1 = 2, rest-of-bindings = {y 1} and <body> = {+ {* x x} y}}. So,
initially we get,
{with {x 2} {with* {{y 1}} {+ {* x x} y}}}

And the new with* is trivially converted to,
{with {x 2} {with {y 1} {+ {* x x} y}}}

Now we convert this to a function application according to,
{with {<id> <ne>} <body>} => {{fun {<id>} <body>} <ne>}

Giving us,
{{fun {x} <body>} 2}

where <body> = {with {y 1} {+ {* x x} y}}. The result of doing the same to body is,
{{fun {y} {+ {* x x} y}} 1}

For a full result of,
{{fun {x} {{fun {y} {+ {* x x} y}} 1}} 2}

Now when we interp this, we interp the function body in a new env containing x -> 2.
Next, we interp the body of the inner function application in an augmented environement
containing y -> 1. The result is a fairly trivial evaluation of {+ {* x x} y}, which 
is 5.

Now we look at the rule used for implementing with* in assignment 2. Using the same
piece of code under:
{with* {{<id1> <ne1>} ... {<idn> <nen>}} <body>} =>
    {{fun {<id1> ... <idn>} <body>} <ne1> ... <nen>}

where id1 = x, ne1 = 2, id2 = y, ne2 = 1 and <body> = {+ {* x x} y}. We get,
{{fun {x y} {+ {* x x} y}} 2 1}

Let's first convert this to a function of one argument,
{{fun {x} {fun {y} {+ {* x x} y}}} 2 1}

But we have to convert this to function applications of single arguments according to,
{<fe> <a1> <rest-of-args>} => {{<fe> <a1>} <rest-of-args>}

where a1 = 2, a2 = 1 and <fe> = {fun {x} {fun {y} {+ {* x x} y}}}. So,
{{{fun {x} {fun {y} {+ {* x x} y}}} 2} 1}

Now we interp the body of the function in an new env containing x -> 1. The body is the
second function application, and we interp it's body in a new augmented env containing
y -> 2. The body is trivially evaluated in this environment yielding {+ {* 1 1} 2},
which is 2.

Because of the order in which functions and function applications are converted to single
argument functions and function applications, the environments in which they are interped
will differ in the order of the named expressions. 

I'm not sure how the order got mixed up but 5.1-d on midterm 1 has a similar situation
in which each argument was evaluated in the same environment, whereas here they are
evaluated in different environments. 

8) {rec ...} and the Y Combinator gave meaning to Paul Graham's essays and startup
incubator. He's a strong advocate of Lisp(s) and the more function programming I do
I start to see why more and more. It's amazing how much you can do with such a small
set of features in a language. Also, RIP Dennis Ritchie and John McCarthy.

