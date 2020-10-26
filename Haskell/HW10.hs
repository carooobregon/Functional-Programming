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

-- === Distance between two points ===

data Point = Point Double Double deriving Show

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- === Complex numbers ===

data Complex = Complex Int Int deriving Show

add :: Complex -> Complex -> Complex
add (Complex a bi) (Complex c di) = (Complex (a+c) (bi+di))

multiply :: Complex -> Int -> Complex
multiply (Complex a bi) c = (Complex (a * c) bi)

getReal :: Complex -> Int
getReal (Complex r _) = r

getImaginary :: Complex -> Int
getImaginary (Complex _ i) = i

-- === The RGB color mode ===

-- Yes, the components are given as a tuple inside the data type...
data RGB = RGB (Int, Int, Int) deriving Show

getR :: RGB -> Int
getR (RGB (r,_,_) ) = r

getG :: RGB -> Int
getG (RGB (_,g,_) ) = g

getB :: RGB -> Int
getB (RGB (_,_,b) ) = b

getMaxComponent :: RGB -> ([Char], Int)
getMaxComponent (RGB (r,g,b)) = if r < g && b < g
                                then ("Green", g)
								else if r < b && g < b
								then ("Blue", b)
								else ("Red", r)

average :: Int -> Int -> Int
average n m = (n + m) `div` 2

combine :: RGB -> RGB -> RGB
combine (RGB (r1,g1,b1)) (RGB (r2,g2,b2)) = (RGB (average r1 r2, average g1 g2, average b1 b2))

-- === Playing cards ===

data Suit = Diamonds | Clubs | Hearts | Spades deriving Show
data Color = Red | Black deriving Show
data Rank = Jack | Queen | King | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving Show
data Card = Card Rank Suit deriving Show

getValue :: Card -> Int
getValue (Card Two _) = 2
getValue (Card Three _) = 3
getValue (Card Four _) = 4
getValue (Card Five _) = 5
getValue (Card Six _) = 6
getValue (Card Seven _) = 7
getValue (Card Eight _) = 8
getValue (Card Nine _) = 9
getValue (Card Ten _) = 10
getValue (Card Jack _) = 11
getValue (Card Queen _) = 12
getValue (Card King _) = 13
getValue (Card Ace _) = 14

getColor :: Card -> Color
getColor (Card _ Diamonds) = Red
getColor (Card _ Clubs) = Black
getColor (Card _ Hearts) = Red
getColor (Card _ Spades) = Black   

-- === Test cases ===

main = do 
    print "=== Distance between two points ==="
    print "=== distance ==="
    print $ distance (Point 10 20) (Point 5 15) -- 7.0710678118654755
    print "=== Complex numbers ==="
    print "=== add ==="
    print $ add (Complex 5 6) (Complex 10 2) -- Complex 15 8
    print "=== multiply ==="
    print $ multiply (Complex 3 4) 6 -- Complex 18 4
    print "=== getReal ==="
    print $ getReal (Complex 2 3) -- 2
    print "=== getImaginary ==="
    print $ getImaginary (Complex 2 3) -- 3
    print "=== The RGB color mode ==="
    print "=== getR ==="
    print $ getR (RGB (150, 30, 25)) -- 150
    print "=== getG ==="
    print $ getG (RGB (150, 30, 25)) -- 30
    print "=== getB ==="
    print $ getB (RGB (150, 30, 25)) -- 25
    print "=== getMaxComponent ==="
    print $ getMaxComponent (RGB (17, 78, 63)) -- ("Green", 78)
    print "=== combine ==="
    print $ combine (RGB (120, 120, 60)) (RGB (150, 70, 80)) -- RGB (135, 95, 70)
    print "=== Playing cards ==="
    print "=== getValue ==="
    print $ getValue (Card King Diamonds) -- 13
    print $ getValue (Card Three Spades) -- 3
    print "=== getColor ==="
    print $ getColor (Card King Diamonds) -- Red
    print $ getColor (Card Three Spades) -- Black