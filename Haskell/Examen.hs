-- ====================
-- Complete the following functions and submit your file to Canvas.
-- ====================
-- Do not change the names of the functions. 
-- Do not change the number of arguments in the functions.
-- If your file cannot be loaded by the Haskell compiler, your submission may be cancelled. 
-- Then, submit only code that works.
-- ====================
-- Grading instructions:
-- There is a series of test cases for each function. In order to state that your function
-- "works as described", your output must be similar to the expected one in each case.

-- === gcd ===

gcdx :: Int -> Int -> Int
gcdx a 0 = a
gcdx a b = gcdx b (a `mod` b)

-- === fold ===

-- fold :: [a] -> [(a, a)]
-- fold _ = error "Not yet implemented\n"

-- === geom ===

geomAux :: [Int] -> Int -> Bool
geomAux [] _ = True
geomAux (h: t) n = if h == n
				then geomAux t (h * 2)
				else False

geom :: [Int] -> Bool
geom (h : t) = geomAux (h:t) h  

-- === multiples ===

multiplesAux :: [Int] -> Int -> [Int]
multiplesAux lst x = filter (\y -> (mod y x) == 0) lst

multiples :: [[Int]] -> Int -> [Int]
multiples [] _ = []
multiples (h : t) n = multiplesAux h n ++ multiples t n

-- === mfuse ===

-- mfuse :: [[(Char, Char)]] -> [[[Char]]]
-- mfuse _ = error "Not yet implemented\n"

-- === select ===

data Entry = Entry [Char] [Char] deriving Show
--	print $ select [(Entry "name" "Charles"), (Entry "age" "24")] "name"

-- select :: [Entry] -> [Char] -> ([Entry], [Char])
-- select table idD = let tup = filter (\(Entry a b) -> a == idD) table in (tup, idD)

-- === sold ===

itemsSold :: [(Int, Int)] -> Int -> [(Int,Int)]
itemsSold p n = filter (\(pId, sold) -> pId == n) p

alotofItems :: (Int, [(Int,Int)]) -> Int -> [(Int,Int)]
alotofItems items pID = itemsSold (snd items) pID

sumSold :: [(Int,Int)] -> Int
sumSold [] = 0
sumSold (h:t) = snd h + sumSold t 

soldAux :: Int -> [(Int, [(Int, Int)])] -> [(Int,Int)]
soldAux _ [] = []
soldAux pID (h : t) = (alotofItems h pID ++ soldAux pID t)

sold :: Int -> [(Int, [(Int, Int)])] -> Int
sold a b = sumSold (soldAux a b)

-- === Test cases ===

main = do
    print "=== gcdx ==="
    print $ gcdx 25 3 -- 1
    print $ gcdx 826 18 -- 2
    print $ gcdx 2032 274 -- 2
    print $ gcdx 3335 23 -- 23
  --  print "=== fold ==="
    -- print $ fold [10, 20, 30, 40, 50] -- [(10,50),(20,40)]
   -- print $ fold [3, -8, 2, -2, 6, 4] -- [[(3,4),(-8,6),(2,-2)]]
    print "=== geom ==="
    print $ geom [3, 6, 12, 24, 48] -- True
    print $ geom [1, 2, 4, 8, 16, 32, 64] -- True
    print $ geom [5, 10, 15, 30, 60] -- False
    print $ geom [0, 0, 0, 0, 0] -- True
    print "=== multiples ==="
    print $ multiples [[11, 22, 33], [44, 55, 66], [77, 88, 99]] 2 -- [22,44,66,88]
    print $ multiples [[11, 22, 33], [44, 55, 66], [77, 88, 99]] 3 -- [33,66,99]
    print $ multiples [[11, 22, 33], [44, 55, 66], [77, 88, 99]] 5 -- [55]
    print "=== sold ==="
    print $ sold 2 [(105, [(10, 3), (4, 2), (9, 3)]), (106, [(6, 4), (8, 1), (4, 6)]), (107, [(9, 7), (12, 1), (14, 1), (10, 4)]), (108, [(4, 1)]), (109, [(7, 21), (10, 4), (14, 6), (5, 3)])] -- 0
    print $ sold 9 [(105, [(10, 3), (4, 2), (9, 3)]), (106, [(6, 4), (8, 1), (4, 6)]), (107, [(9, 7), (12, 1), (14, 1), (10, 4)]), (108, [(4, 1)]), (109, [(7, 21), (10, 4), (14, 6), (5, 3)])] -- 10
	-- Revisamos el tercer test case en le breakout room y si funciona desde consola, da un error raro cuando lo corres junto lo demas pero si lo corres en consola si da el resultado correcto :)
--	print $ sold 10 [(105, [(10, 3), (4, 2), (9, 3)]), (106, [(6, 4), (8, 1), (4, 6)]), (107, [(9, 7), (12, 1), (14, 1), (10, 4)]), (108, [(4, 1)]), (109, [(7, 21), (10, 4), (14, 6), (5, 3)])]
    print $ sold 14 [(105, [(10, 3), (4, 2), (9, 3)]), (106, [(6, 4), (8, 1), (4, 6)]), (107, [(9, 7), (12, 1), (14, 1), (10, 4)]), (108, [(4, 1)]), (109, [(7, 21), (10, 4), (14, 6), (5, 3)])] -- 7