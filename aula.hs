import Data.Char

multiplica :: Int -> [Int] -> [Int]
multiplica _ [] = []
multiplica num (x:xs) = (num * x) : (multiplica num xs)

converte :: String -> [Int]
converte [] = []
converte (x:xs) = (ord x) : (converte xs)

converte2 :: String -> [Int]
converte2 l = [(ord a) |a <- l, (a >= 'a'),(a <= 'z') ]

filtra :: [Int] -> [Int]
filtra l = [a | a <- l, (mod a 2) == 0]

maior :: (Int,Int) -> Int
maior (a,b)
    | a>b = a
    | otherwise = b

maiores :: [(Int,Int)] -> [Int]
--maiores [] = []
--maiores (x:xs) = maior x : maiores xs
maiores l = [maior a | a <- l]

verdadeiro :: [(Bool,Int)] -> [Int]
verdadeiro l = [(snd a) | a <- l, (fst a == True)]

confere :: [(Bool,[Int])] -> [[Int]]
confere l = [confereAUX a | a <- l]

confereAUX :: (Bool,[Int]) -> [Int]
confereAUX (a,b) 
    |a = [ x | x <- b, (mod x 2) /= 0 ]
    |otherwise = b

func1 :: [Int] -> [(Int,Int)]
func1 l = [(a,b) | a <- l, b<-l]

func2 :: [Int] -> [Int] -> [(Int,Int)]
func2 x y = [(a,b) | a <-x, b <-y]

func3 :: [(Int,Int)] -> [(Int,Int)]
func3 x = [(a,b) | (a,b) <- x, a>b]

--Operadores

infixl 7 &&&
(&&&) :: Int -> Int -> Int
a &&& b
    |a > b = a
    |otherwise = b
