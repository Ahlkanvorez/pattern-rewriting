# Pattern Rewriting
<p>
A pattern matching and rewrite-rule system in lisp (being translated into Java) with application to equation solving, symbolic computation, automated theorem proving, translation between programming languages, and grammatical analysis and transformation of sentences. Many of the examples in the code base already are symbolic computation problems, and I intend to add an extended application in parsing and transforming Latin sentences.
</p>

<p>
For more information, <a target="_blank" href="https://www.hrodebert.com/articles/Programming-with-Patterns">check out my blog posts on the subject</a>.
</p>


### How can I see it work?

I've included a short demo program for the lisp/racket implementation, in a file called demo.rkt in the racket/ directory.

Install racket, so that you have the racket terminal command, from the following link: https://download.racket-lang.org/

Next, follow the directions at the top of the demo.rkt file. It currently contains 5 short examples of what the computer algebra system can do.

### Can it solve anything I want?

No. It can do a lot, but it is very limited, as use will show. It is very quick for simple manipulations, but more complicated transformations in the context of trying to solve equations or prove theorems can become extremely cumbersome, as the number of possible expressions to consider becomes innumerable. That being said, if simple transformations are all that are desired, or if searching expression-graphs for expressions matching certain paterns is not desired, the system can be very handy and a lot of fun for experimentation.
