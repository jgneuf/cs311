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

3) I'm starting to see not just how much effort goes into
creating a language, but just how wicked smart (some)
language designers really are. The same goes for guys who
write compilers and interpreters. At the same time, getting
a language to work and realizing it's ACTUALLY a bit of a
language is pretty cool.
