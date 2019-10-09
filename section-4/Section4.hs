module Section4 where

    -- exercise 1

    media::[Double] -> Double
    media a = foldl (+) 0.0 a / fromIntegral(length a)

    -- exercise 2

    palindromo::String -> Bool
    palindromo a = reverse a == a

    palindromos::[String] -> [String]
    palindromos a = filter (palindromo) a

    -- exercise 3
    isPar::Int -> Bool
    isPar a = mod a 2 == 0

    isImpar::Int -> Bool
    isImpar a = mod a 2 == 1

    filtrarPares::[Int] -> [Int]
    filtrarPares a = filter (isPar) a

    filtrarImpares::[Int] -> [Int]
    filtrarImpares a = filter (isImpar) a

    -- Exercise 4 -  Filtre	 os	 números primos de uma lista recebido por parâmetro.
    isPrimo::Int -> Bool
    isPrimo a = length [x | x <- [1..a], mod a x == 0] <= 2

    filterPrimos::[Int] -> [Int]
    filterPrimos a = filter (isPrimo) a

    -- Exercicio 5 - 	Implemente	uma	função	que	receba	uma	lista	de	inteiros	e
    -- retorne	o	dobro	de	todos,	eliminando	os	múltiplos	de	4.

    exercise5::[Int] -> [Int]
    exercise5 = 

    