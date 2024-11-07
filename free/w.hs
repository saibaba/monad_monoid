fn :: Maybe String -> Maybe Integer
fn x = x >>= (\v -> Just (10::Integer))

p :: Maybe Integer -> String
p (Just a) = show a
p (Nothing) = "nada"

xmain = do
  let v = fn (Just "whatever")
  putStrLn (p v)
  putStrLn (p Nothing)
