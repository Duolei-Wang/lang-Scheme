newtype Record a i = Record
  {record :: i -> a}

just :: (Show a) => a -> Record a a
just a = Record $ const a

r1 :: Record Int Int
r1 = just 1

main :: IO ()
main = do
  print $ record r1 100