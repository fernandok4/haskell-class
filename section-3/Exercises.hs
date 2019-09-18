module Exercises where

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