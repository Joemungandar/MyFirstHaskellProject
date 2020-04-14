-- | A Lib module
module Lib
    ( someFunc, square, ggT 
    ) where

-- | Print a String
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Calculate the square of a number
square 
  :: Num a => a -- ^ the number
  -> a -- ^ the square
square a = a * a

-- | Calculate the greatest common divisor
ggT 
  :: Integer -- ^ number a 
  -> Integer -- ^ number b
  -> Integer -- ^ the greatest common divisor
ggT a b 
  | (b < 0) = ggT a (-b) 
  | (a < 0) = ggT (-a) (b)
  | (b == 0) = a
  | (a == 0 || a == b) = b
  | (a > b) =  ggT (a-b) b
  | (a < b) =  ggT a (b-a)
