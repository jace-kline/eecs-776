# Extended Backus-Naur Form (EBNF) Grammar Engine
### Author: Jace Kline

## Purpose

The purpose of this project was to explore the area of parsing in Haskell, particularly through the use of Applicative functors and Monads. I wanted to challenge myself, so I attempted to build a parser for context-free grammars using a version of EBNF syntax. Once a valid grammar is parsed, the next goal was to use the syntax rules of that parser to dynamically build another parser for the language described by the grammar. This turned out to be a very difficult challenge, and the presence of bugs and stalls (parser infinite loops) are to show for it.

## Difficulties

Given the extremely dynamic structure of an EBNF and the arbitrary recursion that can occur, one can not detect infinite loops in any universal/general way. Also, since the languages described by EBNF are generally context-free (and not regular), there exist no general rules for building an efficient parsing automaton. Hence, generating and searching for strings in the language are essentially done via "brute-force" and backtracking. This essentially implies that the computational complexity of such generation and accepter algorithms grows exponentially with a linear increase in grammar size. With this being said, my program generally works for simple grammars with very little complexity.

Another big challenge that I faced was finding a way to display a parse tree to the screen in a "pretty" format, given that the structure of the parse tree for any given grammar is so variable. I ultimately found a solution by using a Grid data type, whereby I would recursively dig in to the structure and store a characters in a grid. On the returns from the recursion, I deployed a combinator function that merged a sub-grid into a larger grid at a specified location on the larger grid. Although this works, the performance is lacking.

## Known Bugs

* In some cases, a displayed parse tree will be missing concatenated terms
* Performing a random string generation or string checks may result in program stalls (infinite generation or search)
  * Ofter occurs when recursive rules are used in the grammar
  * I have used IO forking to attempt to detect and terminate the stalled thread in these scenarios
* Tested strings are occasionally incorrectly deemed as accepted or not accepted by the grammar
* Random generator to generate random strings tends to repeat the same character(s) over and over

## Execution Instructions
1. Install Stack
2. Navigate to the 'ebnf' directory containing the package contents
3. Run `stack build` to build the package
4. Run `stack exec ebnf-exe` to run the build package