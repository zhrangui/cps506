module HighOrder where
   evenList:: Integral a=>[a]->[a]
   evenList = filter even

   squareSum:: Num a => [a] -> a
   squareSum = foldr (\x y -> x * x + y) 0
   
   allSquares::Num a => [a] -> [a]
   allSquares = map (\x -> x * x)

   plus10:: Num a=> [a] -> [a]
   plus10 = scanl (+) 10

   multiply:: Num a => [a] -> [a] -> [a]
   multiply = zipWith (*)
   
   main = do
       el <- evenList [0..10]
       ss <- squareSum [0..10]
       aq <- allSquares [0..10]
       p10 <- plus10 [0..10]
       m <- multiply [1..3] [4..6]
       print el

       