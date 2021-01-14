# Pattern Rewriting
A pattern matching and rewrite-rule system in scheme with application to equation solving, symbolic computation, automated theorem proving, translation between programming languages, and grammatical analysis and transformation of sentences.

The repository also includes an incomplete Java rewrite of the system, which I currently have no plans to complete.

## Demo

See `racket/demo.rkt` for some usage examples; you'll need to install [Racket](https://racket-lang.org) to run the files.

## About

I wrote this in college as a fun hobby project, but haven't improved it since. It traverses the expression graph naively, so equations where the solution is a long distance from the original problem will have significantly longer runtimes than those of a shorter path length. It can offer step by step solutions to basic algebraic problems, and when given the right rewrite rules, can provide proofs of basic results in set theory. It can also be used for rewriting lisp code based on structural patterns.

## License

MIT License

Copyright (c) 2016 Robert Mitchell

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
