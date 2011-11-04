Student Name: Jonathan Neufeld		
Student Number: 30671093	
CS ID: p9d8

Collaboration Partner Name: Afsha Sethna			
Student Number: 54780085	
CS ID: c0x6

"I have read and complied with the collaboration policy."


1. When we clone a tab we create a clone of the global hash table and bring it along 
with us. Then when we go back to answer the first question we set the first element in
the hash table to "No". The web app will then send us to the last question -- but we're
taking every other element of the hash table with us as well, i.e. all the answers for
the other questions. After answering the last question and printing the results, we 
print the results for all the other questions as well, since they're in the hash table.
	
2. Open the base URL, click 'No' and clone the tab twice. In both of the cloned tabs we
can click 'Back' in the browser to the first question and choose 'Yes'. Now we can
continue the survey and get very different results in all three tabs. But when we went
back to the first question in the duplicate tabs (tabs two and three), we invoked the
same continuation twice.

Without using tabs, we could answer the first three questions, then go back to the 
second question and change our answer. After answering a couple more questions, we 
could again go back to the second question and change our answer. Now we have invoked 
the same continuation twice from a single tab, changing the later answers in both cases.

3. If we answer 'Yes' to the first question and 'Italy' to the second question, then go
back to the first question and answer 'No', we create a continuation for question two,
i.e. 'Italy', but since we go back to question one and skip ahead to question seven we
never actually invoke the continuation for question two.

4. I haven't seen anything other than PHP serverside before, looking at Racket act as
both the webserver and an app (in this case a survey, in class a primitive message
board) was interesting. It makes me want to try Ruby/on Rails, Python/Django or
Scala/Lift.

Bonus: There's some nice error checking on our web app.
