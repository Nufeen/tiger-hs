# Tiger book exercises

Work in progress

## Nix notes

https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.htm

https://www.haskell.org/cabal/users-guide/developing-packages.html#using-cabal-init

See `Makefile` for usage

## Notes

Tiger Compiler Reference Manual: https://www.lrde.epita.fr/~tiger/tiger.html

ML sources: https://www.cs.princeton.edu/~appel/modern/ml/

Testcases:

```
wget -r 1 -A 'test*.tig' https://www.cs.princeton.edu/~appel/modern/testcases/
```

### Lexer

**Possible approaches for Haskell**:


Alex (platform part) way:

https://www.haskell.org/alex/ (most github tiger compilers use it)

The BNF Converter: https://github.com/BNFC/bnfc (BNF grammar -> Alex generator file)

http://dev.stephendiehl.com/fun/008_extended_parser.html

Reason to search for alternatives: *The code in each of these modules is a hybrid of the specific Alex/Happy grammar syntax and arbitrary Haskell logic that is spliced in. Code delineated by braces ({}) is regular Haskell, while code outside is parsera and lexer logic.*


Parsec way:

http://dev.stephendiehl.com/hask/#custom-lexer

http://dev.stephendiehl.com/fun/002_parsers.html


Applicative parser combinators:

https://ro-che.info/articles/2015-01-02-lexical-analysis

http://hackage.haskell.org/package/lexer-applicative-2.1.0.2/docs/Language-Lexer-Applicative.html

Uses https://github.com/feuerbach/regex-applicative that seems to be slower

*For common tasks, this package is several times slower than monadic parser combinator libraries like parsec.*
