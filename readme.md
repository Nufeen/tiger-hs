# Tiger book exercises

Work in progress


## Tiger notes

Tiger Compiler Reference Manual: https://www.lrde.epita.fr/~tiger/tiger.html

ML sources: https://www.cs.princeton.edu/~appel/modern/ml/

Testcases:

```
wget -r 1 -A 'test*.tig' https://www.cs.princeton.edu/~appel/modern/testcases/
```

### Lexer

To check the lexer outut on testcases run in nixshell:

```
cabal run tests.lexer
```

**Possible approaches for Haskell**:

#### Alex (platform part) way:

https://www.haskell.org/alex/ (most github tiger compilers use it)

The BNF Converter: https://github.com/BNFC/bnfc (BNF grammar -> Alex generator file)

http://dev.stephendiehl.com/fun/008_extended_parser.html

Reason to search for alternatives: *The code in each of these modules is a hybrid of the specific Alex/Happy grammar syntax and arbitrary Haskell logic that is spliced in. Code delineated by braces ({}) is regular Haskell, while code outside is parsera and lexer logic.*


#### Parsec way:

http://dev.stephendiehl.com/hask/#custom-lexer

http://dev.stephendiehl.com/fun/002_parsers.html


#### Applicative parser combinators (used in this realisation):

https://ro-che.info/articles/2015-01-02-lexical-analysis

http://hackage.haskell.org/package/lexer-applicative-2.1.0.2/docs/Language-Lexer-Applicative.html

Uses https://github.com/feuerbach/regex-applicative that seems to be slower

*For common tasks, this package is several times slower than monadic parser combinator libraries like parsec.*

https://github.com/feuerbach/regex-applicative/wiki/Examples


### Parser

https://www.lrde.epita.fr/~tiger/tiger.html#Syntactic-Specifications


**Possible approaches for Haskell**:

Platform way: https://www.haskell.org/happy/


Parser combinators:

Since Tiger grammar suites LL(k) form we can use them:

http://dev.stephendiehl.com/fun/002_parsers.html



Earley Parsing:

http://hackage.haskell.org/package/Earley-0.12.0.0/docs/Text-Earley.html

http://loup-vaillant.fr/tutorials/earley-parsing/


## Nix notes

Project is organized this way: https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.htm


## Testing:

To run all:

```
make tests
```

Possible command (verbose, for testing certain parts) for instant test run inside nix shell (too slow because of rebuilds):

```
ghcid -T ':!cabal run tests.exercises'
```

Other possible way (slow too):

```
ghcid -T ':!cabal test'
```

See https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites and https://github.com/ndmitchell/ghcid/issues/65
