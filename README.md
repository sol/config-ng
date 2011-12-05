## Using attoparsec
You can build this library against
[attoparsec](http://hackage.haskell.org/package/attoparsec) instead of
[Parsec](http://hackage.haskell.org/package/parsec).  This is somewhat faster,
but comes at the cost of less useful error messages on parse errors.  To enable
this, pass `-f use-attoparsec` to `cabal configure`.
