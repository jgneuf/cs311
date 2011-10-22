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

3) 

4) 
{with {x {read-eval}}
  x}
In this case there's obviously no way to predict the value of x, since it depends on
user input. 

{with {complex-computation {fun {n} ...}}
  {with {x {complex-computation 10}}
    x}}
Consider the second with. At the time when it is being evaluated, we don't know what x
is bound to, we don't what the result of {complex-computation 10} is at all. This isn't
because we've conveniently been shown the ... in place of the function body, we know
this because complex-computation is bound to a closureV rather than a value, something
that produces another value later on.

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
Dynamic: Error unbound identifier countdown in the rhs of the binop.

6) 
