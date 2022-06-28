import Data.List ( intersperse )
--  Задание 1.
properDivisors1 :: Integer -> [Integer]
properDivisors1 n = [x | x <- [1..n `div` 2], n `mod` x == 0]

properDivisors2 :: Integer -> [Integer]
properDivisors2  n = properDivisors2' 1
    where properDivisors2' :: Integer -> [Integer]
          properDivisors2' i 
                            | i*i > n = []
                            | i*i == n = [i]
                            | i == 1 = (i:result)
                            | n `mod` i == 0 = (i: (n `div` i):result)
                            | otherwise = result
                            where result = properDivisors2' (i+1)

perfect :: Integer -> Bool
perfect x = sum (properDivisors1 x) == x

findPerfects :: Integer -> [Integer]
findPerfects x = [y | y <-[1..x], perfect y]
-- *Main> findPerfects 1000
-- [6,28,496]

-- Задание 2.
abbrev :: String -> String
abbrev name  
        | null name_list  = []
        | length name_list == 1 = name
        | otherwise = intersperse '.' first_letters ++ ['.'] ++ last name_list
        where name_list = words name
              first_letters = [head x| x <- init name_list]

---Задание 3
approxPi :: [Double]
approxPi = scanl1 (+) [ 4*(-1)**i/(2*i+1) | i<-[0..] ]

delta :: [Double] -> [Double]
delta x = zipWith (-) (tail x) x

euler :: [Double] -> [Double]
euler a = zipWith (\x y -> (head x) * y) deltas degrees_of_two
          where deltas = iterate delta a
                degrees_of_two = iterate (/2) 0.5

fastApproxPi :: [Double]
fastApproxPi = scanl1 (+) (zipWith (*)  [ (-1)**i | i<-[0..] ] (euler a))
               where a = [ 4/(2*i+1) | i<-[0..] ]

-- Задание 4
data Exp =
  Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
        deriving (Show, Eq)

instance Num Exp where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Const . fromInteger
  abs = undefined
  signum = undefined

applyDistr :: Exp -> Exp
-- Addition
applyDistr (Mul (Add e1 e2) e) = applyDistr $ Add (Mul (applyDistr e1) (applyDistr e)) (Mul (applyDistr e2) (applyDistr e))
applyDistr (Mul e (Add e1 e2)) = applyDistr $ Add (Mul (applyDistr e) (applyDistr e1)) (Mul (applyDistr e) (applyDistr e2))
--Subtraction
applyDistr (Mul (Sub e1 e2) e) = applyDistr $ Sub (Mul (applyDistr e1) (applyDistr e)) (Mul (applyDistr e2) (applyDistr e))
applyDistr (Mul e (Sub e1 e2)) = applyDistr $ Sub (Mul (applyDistr e) (applyDistr e1)) (Mul (applyDistr e) (applyDistr e2))

applyDistr (Add e1  e2) =  Add (applyDistr e1) (applyDistr e2)
applyDistr (Sub e1  e2) =  Sub (applyDistr e1) (applyDistr e2)
applyDistr (Mul e1 e2) =   Mul (applyDistr e1) (applyDistr e2)

applyDistr e = e

--Задание 5
-- z :: a
-- x :: b
-- y :: a
-- xs :: [b]
-- f :: a -> b -> a
-- f y x :: a -> a
-- g :: a -> a
-- g (f y x) :: a -> a
-- id :: a -> a
-- (\x g y -> g (f y x)) :: (b -> (a->a) -> a -> (a -> a))
-- foldr (\x g y -> g (f y x)) id xs z :: (b -> (a->a) -> a -> (a -> a)) -> (a->a) -> [b] -> a -> a

--Задание 6
concatS :: [ShowS] -> String
concatS diff_list = foldMap ($) diff_list $ ""

main :: IO ()
main = do
    print("----------------Task 1. ----------------")
    print(findPerfects 1000)
    print("----------------Task 2. ----------------")
    print(abbrev "")
    print(abbrev "Name1")
    print(abbrev "Name1 Name2")
    print(abbrev "Name1 Name2 Name3")
    print(abbrev "Name1 Name2 Name3 Name4")
    print("----------------Task 3. ----------------")
    print(take 10 approxPi)
    print(take 10 fastApproxPi)
    print("----------------Task 4 ----------------")
    print("(1+2)*3) = " ++ show (applyDistr((1+2)*3)))
    print("3*(1+2) = " ++ show (applyDistr(3*(1+2))))
    print("----------------Task 6 ----------------")
    print(concatS (map make_diff_list ["123","456","789"]))
            where make_diff_list s = (s++)
