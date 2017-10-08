

foo :: String -> Int -> Bool
foo i _
  | (length i) < 5 = False
foo [] _ = False
foo (_ : _) j = j < 5


-- λ> foo "" 2
-- False
-- λ> foo "fdafdsdas" 2
-- True
-- λ> foo "fdafdsdas" 7
-- False
-- λ> 
