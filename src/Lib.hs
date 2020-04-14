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
ggT a b = 
  if b < 0
    then ggT a (-b) 
    else if a < 0
      then ggT (-a) (b)
      else if b == 0
        then a
        else if a == 0 || a == b
          then b
          else if a > b
            then ggT (a-b) b
            else ggT a (b-a)
