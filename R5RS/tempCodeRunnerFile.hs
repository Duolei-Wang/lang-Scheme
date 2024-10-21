fn :: Int -> Int -> Int
fn a b = a + b

lst = [fn 1, fn 2]

main = do
  print $ map ($ 10) lst
  print $ ($ 10) (fn 1)