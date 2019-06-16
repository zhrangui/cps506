module HighOrder where
   evenList:: Integral a => [a]->[a]
   evenList = filter even

   squareSum:: Num a => [a] -> a
   squareSum = foldr (\x y -> x * x + y) 0
   
   allSquares::Num a => [a] -> [a]
   allSquares = map (\x -> x * x)

   plus10:: Num a => [a] -> [a]
   plus10 = scanl (+) 10

   multiply:: Num a => [a] -> [a] -> [a]
   multiply = zipWith (*)

   div5:: Integral a => [a] -> [a]
   div5 = takeWhile (\x -> x `div` 5 == 0)
   
   main = do
       let el = evenList [0..10]
       let ss = squareSum [0..10]
       let aq = allSquares [0..10]
       let p10 = plus10 [0..10]
       let m = multiply [1..3] [4..6]
       let d = div5 [1..20]
       putStrLn "Filter Even number:" 
       print el
       putStrLn "Sum squared list:" 
       print ss
       putStrLn "Squared list:" 
       print aq
       putStrLn "Add 10 to list:" 
       print p10
       putStrLn "Two lists product each element:" 
       print m
       putStrLn "Find residue of 5 in list:" 
       print d
       
       

       