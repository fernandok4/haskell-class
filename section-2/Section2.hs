module Section2 where

    fatorial :: (Int) -> Int
    fatorial 0 = 1
    fatorial x = x * fatorial(x - 1)

    -- dei uma viajada aqui e fiquei fazendo o triangulo de pascal

    -- binomioNewton :: (Int, Int) -> Int
    -- binomioNewton (n, m) = quot (fatorial(n)) (fatorial(m) * (fatorial((n - m))))

    -- pascalTringleLine :: (Int) -> String
    -- pascalTringleLine (n) = concat([show(binomioNewton(n, x)) | x <- [0..n]])

    -- pascalTringle :: (Int) -> [Int]
    -- pascalTringle (n) = [read(pascalTringleLine(x)) | x <- [0..n]]

    a1 :: [Int]
    a1 = [11 ^ x | x <- [1..6]]

    b1 :: [Int]
    b1 = [x | x <- [1..40]]

    c1 :: [String]
    c1 = ['A' : x : "BB" | x <- "abcdefg"]

    d :: [Int]
    d = [2 + (3 * x) | x <- [1..12]]

    e1 :: [Double]
    e1 = [1 / (2 ^ x) | x <- [0..6]]

    f1 :: [Int]
    f1 = [9 * x + 1 | x <- [0..7]]

    g1 :: [Int]
    g1 = [2 * x | x <- [1..15], notElem x [3, 7, 10, 13]]

    h1 :: [Char]
    h1 = [x | x <- "@ACDEGJL"]

    exercise2 :: String -> Bool
    exercise2 x = (mod (length x) 2) == 0

    exercise3 :: [String] -> [String]
    exercise3 xs = [reverse x | x <- xs]

    exercise4 :: [String] -> [Int]
    exercise4 xs = [length x | x <- xs, not (exercise2 x)]

    exercise5 :: [a] -> a
    exercise5 xs = last (reverse xs)

    exercise6 :: String -> Bool
    exercise6 x = x == reverse x

    exercise7 :: Int -> (Int, Int, Int, Int)
    exercise7 x = (x * 2, x * 3, x * 4, x * 5)



