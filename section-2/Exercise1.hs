module Exercise1 where

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

    -- d :: [Int]

    e1 :: [Double]
    e1 = [1 / (2 ^ x) | x <- [0..6]]

    f1 :: [Int]
    f1 = [9 * x + 1 | x <- [0..7]]

    g1 :: [Int]
    g1 :: []



