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

7)
{with* {{x 1} {x 2}} {+ x y}}

A2:
{f 1 2} -> {{f 1} 2}

{fun {x y} {+ x y}} -> {fun {x} {fun {y} {+ x y}}}

{{ {fun {x} {fun {y} {+ x y}}} 1} 2}

A1:
{with* {{x 1} {y 2}} {+ x y}} -> {with {x 1} {with {x 2} {+ x y}}}

{with {x 1} {with {y 2} {+ x y}}} -> {{fun {x} {nested-with}} 1}

{{fun {x} {{fun {y} {+ x y}} 2} 1}

8) {rec ...} and the Y Combinator gave meaning to Paul Graham's essays and startup
incubator. He's a strong advocate of Lisp(s) and the more function programming I do
I start to see why more and more. It's amazing how much you can do with such a small
set of features in a language. Also, RIP John McCarthy.

