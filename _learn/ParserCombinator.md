= Parser Combinator in Haskell

==  Record a successful action instead only result.


== Functor on a Container

== Applicative

== Monad

== Alternative


== many

many p => *(p)

many1 p => p >> *(p)

== sepBy

sepBy p sep => *(p *(sep >> p))
sepBy1 p sep => (p *(sep >> p)) >> *(p *(sep >> p))

== endBy

endBy p sep => *(p >> sep)
endBy1 p sep => (p >> sep) >> *(p >> sep)

