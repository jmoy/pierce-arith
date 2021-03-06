Implementation of simple arithmetic expressions from
Chapter 3 of Pierce's 'Types and Programming Languages'
in Haskell.

The evaluator is a big-step evaluator in Pierce's 
terminology.

The package provides an executable `arith`, which presents
the user with a REPL at which arithmetic expressions can be
typed. The sytem prints out a parse tree and the value
of the expression.

It was an experiment for me to see how to implement
a little language in Happy and Alex. In particular it uses
monadic lexers and parsers in order to gracefully recover from 
errors.

We also demonstrate the StateT monad transformer and Error
monad from the mtl library.

If you have Cabal installed you can build with

    cabal build

and run with

    cabal run
Otherwise see instructions for building [here](http://www.haskell.org/ghc/docs/7.0.2/html/Cabal/builders.html).

Please send your comments to jyotirmoy@jyotirmoy.net
