- ($) :: (a -> b) -> a -> b
- ($ a) :: (a -> b) -> b
  ($ x) f => f x
  Some kind of x |> f.

- data: Val -> Type.
- type: Product, Sum of type.
- newtype: build type with constructors.

- size n@(Node _ _ _)   = size (left n) + 1 + size (right n)