= Monad

== Functor

For those implementing Functors with type "t a", we can get the result of a map "a -> b" into it to derive "t b". The fmap(or infix <$>) provides this action.

```hs
fmap f (t a) = t (f a)
f <$> t a = t (f a)
```

== Applicative

The applicative means "t f", with "f : a -> b" act on "t a" can get "t b". The infix is the symbol "<*>". One should notice that

```hs
f :: a -> b -> c
f <$> t a <*> t b = t (f a) <*> t b = t (f a b)
```

That's how we use monad mostly