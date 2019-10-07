module Section3 where

    -- Exercicio 1

    data Pergunta = Sim | Nao deriving (Show)

    pergNum :: Pergunta -> Int
    pergNum Sim = 1
    pergNum Nao = 0

    listPergs :: [Pergunta] -> [Int]
    listPergs xs = [pergNum x | x <- xs]

    and' :: Pergunta -> Pergunta -> Pergunta
    and' Sim Sim = Sim
    and' Nao Sim = Nao
    and' Sim Nao = Nao
    and' Nao Nao = Nao

    or' :: Pergunta -> Pergunta -> Pergunta
    or' Sim Sim = Sim
    or' Nao Sim = Sim
    or' Sim Nao = Sim
    or' Nao Nao = Nao

    not' :: Pergunta -> Pergunta
    not' Sim = Nao
    not' Nao = Sim

    -- Exercicio 2 - Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kevin. Implemente as funções:
    
    -- converterCelsius: Recebe um valor double e uma temperatura, e faz a conversão para Celcius

    -- converterKelvin: recebe um valor double e uma temperatura, e faz a conversão para Kelvin

    -- converterFarenheit: recebe um valor double e uma temperatura e faz a conversão para Farenheit

    data Temperatura = Celcius | Farenheit | Kelvin deriving (Show)
    
    temperaturaBase :: Temperatura -> (Double, Double)
    temperaturaBase (Celcius) = (0, 100)
    temperaturaBase (Farenheit) = (32, 212)
    temperaturaBase (Kelvin) = (273, 373)
    
    converterCelsius :: Double -> Temperatura -> (Double, Temperatura)
    converterCelsius x y = converteEscala x y Celcius
    
    converterKelvin :: Double -> Temperatura -> (Double, Temperatura)
    converterKelvin x y = converteEscala x y Kelvin
    
    converterFarenheit :: Double -> Temperatura -> (Double, Temperatura)
    converterFarenheit x y = converteEscala x y Farenheit
    
    converteEscala :: Double -> Temperatura -> Temperatura -> (Double, Temperatura)
    converteEscala graus deTemp paraTemp = ((((graus - minDeTemp) * (maxParaTemp - minParaTemp) / (maxDeTemp - minDeTemp)) + minParaTemp), paraTemp)
        where minDeTemp = fst(temperaturaBase(deTemp))
              maxDeTemp = snd(temperaturaBase(deTemp))
              minParaTemp = fst(temperaturaBase(paraTemp))
              maxParaTemp = snd(temperaturaBase(paraTemp))


    
    -- Exercicio 3 - Implement uma funçao que simule o vencedor de uma partida de pedra, papel e tesoura usando tipos craidos. Casos de empate devem ser considerados em seu tipo.

    data OpcoesJogo = Pedra | Papel | Tesoura deriving (Enum, Eq, Ord)
    data ResultadoJogo = Venceu | Perdeu | Empate deriving (Show)

    jogarPedraPapelTesoura :: OpcoesJogo -> OpcoesJogo -> (ResultadoJogo, ResultadoJogo)
    jogarPedraPapelTesoura Tesoura Pedra = (Perdeu, Venceu)
    jogarPedraPapelTesoura Pedra Tesoura = (Venceu, Perdeu)
    jogarPedraPapelTesoura opc1 opc2 | opc1 == opc2 = (Empate, Empate)
    jogarPedraPapelTesoura opc1 opc2 | opc1 > opc2 = (Venceu, Perdeu)
    jogarPedraPapelTesoura opc1 opc2 | opc1 < opc2 = (Perdeu, Venceu)

    -- Exercicio 4 - Faça uma função que retorne uma string com todas as vogais maiusculas e minusculas eliminadas de uma string passada por parametro usando list compreenshion

    removeConsoantes :: String -> String
    removeConsoantes palavra = [x | x <- palavra, elem x "aeiouAEIOU"]

    -- Exercise 5 

    data UnidadeMedida = Inch | Yard | Foot deriving (Show)

    converterMetros :: (Double, UnidadeMedida) -> Double
    converterMetros (x, Inch) = 0.0254 * x
    converterMetros (x, Yard) = 0.9144 * x
    converterMetros (x, Foot) = 0.3048 * x

    converterImperial :: (Double, UnidadeMedida) -> (Double, UnidadeMedida)
    converterImperial (x, Inch) = (x / 0.0254, Inch)
    converterImperial (x, Yard) = (x / 0.9144, Yard)
    converterImperial (x, Foot) = (x / 0.3048, Foot)

    -- Exercicio 6

    data Mes =  Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro |
                Outubro | Novembro | Dezembro deriving (Eq, Enum, Show)

    data Hemisferio = Norte | Sul deriving (Eq)

    data Estacao = Verao | Outono | Primavera | Inverno deriving (Show)

    checaFim :: Mes -> Int
    checaFim x 
        | elem x [Janeiro, Marco, Maio, Julho, Agosto, Outubro, Dezembro] = 31
        | elem x [Abril, Junho, Setembro, Novembro] = 30
        | elem x [Fevereiro] = 28

    prox :: Mes -> Mes
    prox x
        | elem x [Janeiro, Fevereiro, Marco, Abril, Maio, Junho, Julho, Agosto, Setembro, Outubro, Novembro] = toEnum (fromEnum x + 1)
        | x == Dezembro = Janeiro

    estacao :: (Mes, Hemisferio) -> Estacao
    estacao x 
        | elem (fst x) [Janeiro, Fevereiro, Marco] && (snd x) == Sul = Verao 
        | elem (fst x) [Abril, Maio, Junho] && (snd x) == Sul = Primavera 
        | elem (fst x) [Julho, Agosto, Setembro] && (snd x) == Sul = Inverno 
        | elem (fst x) [Outubro, Novembro, Dezembro] && (snd x) == Sul = Outono 
        | elem (fst x) [Janeiro, Fevereiro, Marco] && (snd x) == Norte = Inverno 
        | elem (fst x) [Abril, Maio, Junho] && (snd x) == Norte = Outono 
        | elem (fst x) [Julho, Agosto, Setembro] && (snd x) == Norte = Verao 
        | elem (fst x) [Outubro, Novembro, Dezembro] && (snd x) == Norte = Primavera 


    -- Exercicio 7
    verificaPalindromo :: String -> Bool
    verificaPalindromo x = reverse x == x 

    -- Exercicio 8
    eliminaNums :: [Int] -> [Int]
    eliminaNums xs = [x | x <- reverse xs, x > 0, mod x 2 == 1, mod x 7 /= 0]

    -- Exercicio 9
    groupTuple :: String -> String -> String -> (String, String, String)
    groupTuple x y z = (reverse x, reverse y, reverse z)

    -- Exercicio 10
    revNum :: String -> Int -> Maybe String
    revNum xs n 
        | length xs <= n = Nothing
        | otherwise = Just ((reverse (take n xs)) ++ reverse (take ((length xs) - n) (reverse xs)))

    -- Exercicio 11
    data Binario = Zero | Um