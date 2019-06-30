module Lib
    ( distance
    ) where

distance :: Floating a => [a] -> [a] -> a
distance x y = sum (zipWith (\a b -> sqrt (a*a + b*b)) x y)

main :: IO ()
main = do
    putStrLn "Distance of multiple dimensions: "
    print (distance [1,2,3] [4,5,6])