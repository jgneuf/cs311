Name: Jonathan Neufeld
Student ID: 30671093

I have read and complied with the collaboration policy.

1)

2) Here's an interesting situation:
	{with* {{x 5} {y {+ x x}}} {+ x y}}
The result in assignment one is 15. But using the fun
translation given we get:
	{{fun {x y} {+ x y}} 5 {+ x x}}
When {+ x x} is applied to the function, it does not find
a binding for x, since it is being evaluated in a different
environment.

3) "null" usually means nothing. When I put null on my last
assignment it really was an answer to the question "what did
this assignment make you think of?" -- I didn't really think
of anything. This assignment made me think I need to be far
more verbose so I don't lose any small marks.
