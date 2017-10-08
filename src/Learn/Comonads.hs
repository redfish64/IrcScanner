module Learn where


data U x = U [x] x [x]

right :: U x -> U x
right (U a b (c:cs)) = U (b:a) c cs
right _ = undefined

left :: U x -> U x
left  (U (a:as) b c) = U as a (b:c)
left _ = undefined

instance Functor U where
   fmap f (U a b c) = U (map f a) (f b) (map f c)

class Functor w => Comonad w where
    (=>>)    :: w a -> (w a -> b) -> w b
    coreturn :: w a -> a
    cojoin     :: w a -> w (w a)
    x =>> f = fmap f (cojoin x)

instance Comonad U where
   cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
   coreturn (U _ b _) = b

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True


rule :: U Bool -> Bool   
rule (U (a:_) b (c:_)) = foldr xor False [a,b,c]
rule _ = undefined

shift :: Int -> U x -> U x
shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList :: Int -> Int -> U a -> [a]
toList i j u = take (j-i) $ half $ shift i u where
   half (U _ b c) = [b] ++ c

test :: IO ()
test = let u = U (repeat False) True (repeat False)
      in putStr $
         unlines $
         take 20 $
         map (map (\x -> if x then '#' else ' ') . toList (-20) 20) $
         iterate (=>> rule) u
