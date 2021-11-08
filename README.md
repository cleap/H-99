# H-99: Ninety-Nine Haskell Problems

These are my solutions to [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems),
which are translations of [Ninety-Nine Lisp Problems](https://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html),
which are themselves translations of [Ninety-Nine Prolog Problems](https://web.archive.org/web/20170324220754/https://sites.google.com/site/prologsite/prolog-problems).

## Why should I take your word for it?

We can test the solutions using [HUnit](https://github.com/hspec/HUnit):

```bash
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude> :load grade
[1 of 2] Compiling Unit01           ( Unit01.hs, interpreted )
[2 of 2] Compiling Main             ( grade.hs, interpreted )
Ok, two modules loaded.
Prelude> runTestTT unit01
Cases: 10  Tried: 10  Errors: 0  Failures: 0
Counts {cases = 10, tried = 10, errors = 0, failures = 0}
```
