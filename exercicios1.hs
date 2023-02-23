import Data.Char

--Exercicio 1)Declare, em Haskell, as funções abaixo, contemplando, também, os protótipos (cabeçalhos)
type X = Double
type Y = Double
type Z = Double

funcaoUm :: X -> Double
funcaoUm x
    |x >= 0 = (x+4)/(x+2)
    |otherwise = 2/x

funcaoDois :: X -> Y -> Double
funcaoDois x y 
    |x >= y = x + y
    |otherwise = x - y

funcaoTres :: X -> Y -> Z -> Double
funcaoTres x y z
    |(x+y) > z = x + y + z
    |(x+y) < z = x - y - z
    |(x+y) == z = 0

--Exercicio 2)Localize, explique e corrija o erro na função que deve calcular o fatorial de um número, como se segue:
		--fat::Int->Int
		--fat x = x * fat(x-1
fat::Int->Int
fat 0 = 1 --faltava a base como critério de parada
fat x = x * fat(x-1)

--Exercicio 3)Considere a função em Haskell soma::Int->Int->Int que retorna a soma entre os dois parâmetros. Assim, faça uma função em 
--Haskell que resulte a multiplicação de dois parâmetros fazendo uso da função soma
soma::Int->Int->Int
soma x y = x + y

multi :: Int -> Int -> Int
multi _ 0 = 0
multi x y = soma x (multi x (y-1))

--Exercicio 4)Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um número inteiro
listSize::[dataType]->Int
listSize   [] = 0 
listSize  (h:t) = 1 + listSize t

listToInt::[Int]->Int
listToInt  [] = 0
listToInt  (h:t) = (h * 10^(listSize t)) + listToInt t 

reverseList::[Int]->[Int]
reverseList   [] = []
reverseList   (h:t) = (reverseList t) ++ h:[]

createList::Int->[Int]
createList   0 = []
createList   x = createList (div x 10) ++ (mod x 10):[]

invertInt::Int->Int
invertInt   x 
        | x >=0 = listToInt (reverseList (createList x))
        | otherwise = (-1) * listToInt (reverseList (createList (abs x)))


--Exercicio 5)Escreva, em Haskell, a definição de uma função fourPower que retorne o seu argumento elevadoà quarta potência.  
--Use a função square dada em sala de aula na definição de fourPower
square :: Int -> Int -- função que eleva um número ao quadrado
square x = x * x

fourPower :: Int -> Int
fourPower x = (square x) * (square x) 


--Exercicio 6)Considere a sequência: √6;√(6 +√6);√[6 +√(6 +√6)]; ...; com tendência ao +∞. Faça, em Haskell,uma função para calcular o  
--i−ésimo termo desta sequência, considerando i0 =√6
raizSeis :: Int -> Double
raizSeis iesimo
    |iesimo > 0 = sqrt( 6 + raizSeis (iesimo-1))
    |otherwise = 0

--Exercicio 7)Escreva, em Haskell, uma função que informa de quantas maneiras é possível escolher n objetos em uma coleção original de m  
--objetos, para m ≥ n
combinacao :: Int -> Int -> Int
combinacao m n = div (fat m) ((fat n) * (fat (m - n)))

--Exercicio 8)Considere a função escrita na linguagem C que calcula o máximo denominador comum entre dois números
{-int mdc(int m, int n) {
          while ((m \% n) != 0) {
              int aux = m;
              m = n;
              n = aux \% n ;
          }
          return n;
      }-}
mdc :: Int -> Int -> Int 
mdc m n 
    |mod m n /= 0 = mdc n (mod m n)
    |otherwise = n

--Exercicio 9)Escreva, em Haskell, uma função que retorna quantos múltiplos de um determinado inteiro tem em um intervalo fornecido. 
--Por exemplo, o número 4 tem 2 múltiplos no intervalo de 1 a 10
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples num inicio fim
    |((num * inicio) <= fim) && (inicio <= fim) = 1 + (howManyMultiples num (inicio+1) fim)
    |inicio <= fim = howManyMultiples num (inicio+1) fim
    |otherwise = 0

--Exercicio 10)Escreva, em Haskell, uma função que retorna o último dígito de um número inteiro
lastDigit :: Int -> Int
lastDigit x = mod x 10

--Exercicio 11)Escreva, em Haskell, uma função que retorna o dígito de um número inteiro de acordo com a posição informada
tamanhoNum :: Int -> Int
tamanhoNum 0 = 0
tamanhoNum x = 1 + tamanhoNum (div x 10)

anyDigit :: Int -> Int -> Int
anyDigit posicao num
    |posicao >= tamanhoNum num = -1
    |otherwise = div (mod num (10^a) - mod num (10^(a-1))) (10^(a-1))
    where
        a = tamanhoNum num - posicao


{-Exercicio 12)Um programador especificou a função allDifferent para identificar se três números inteiros são todos diferentes entre si, 
da seguinte forma: 
      allDifferent::Int->Int->Int->Bool
      allDifferent m n p = (m/=n) && (n/=p) -}
--(a) O que está errado nessa definição?
--Está errado nesta definição o fato de não haver uma terceira comparação entre m e p pois m sendo diferente de n e n diferente de p não garante q m é diferente de p 

--b)
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)

{-Exercicio 13)Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes)-}
howManyEqual::Int->Int->Int->Int
howManyEqual a b c 
    |a==b && b==c = 3
    |a==b && b/=c || b==c && a/=c || a==c && c/=b = 2
    |otherwise = 0

--Exercicio 14)Para o exemplo da função sales::Int->Int dada em sala de aula faça o que se pede,
{-(a) Implemente a função howManyLess que calcule quantos dias as vendas foram inferiores a um dado valor, dentro de um intervalo 
de dias dentro do período total. O primeiro parâmetro de howManyLess indica o valor mínimo de vendas, o segundo parâmetro indica
o dia do início do intervalo e o terceiro parâmetro é o dia do fim do intervalo desejado dentro do período total de dias da função-}

{- (b) Implemente a função noZeroInPeriod::Int->Bool que retorna True somente se não há nenhum dia no período em que o número de 
vendas da função sales foi zero. -}


{- (c) Implemente a função zerosInPeriod::[Int] que retorne a lista de todos os dias em que as vendas foram de zero unidades; -}

{- (d) Utilizando listas de inteiros, retorne os dias em que as vendas foram abaixo de um determinado valor passado como parâmetro;
-}

vendas :: Int -> Int
vendas 1 = 8
vendas 2 = 72
vendas 3 = 41
vendas 4 = 2
vendas 5 = 91
vendas 6 = 30
vendas 7 = 66
vendas _ = 0

--a)
howManyLess::Int->Int->Int->Int
howManyLess minimo inicio fim 
    |(minimo > vendas inicio) && (inicio <= fim) = 1 + (howManyLess minimo (inicio+1) fim)
    |(inicio <= fim) = (howManyLess minimo (inicio+1) fim)
    |otherwise = 0

--b)
noZeroInPeriod::Int->Bool
noZeroInPeriod periodo
    |(vendas periodo == 0) && (periodo > 0) = False
    |(vendas periodo /= 0) && (periodo > 0) = noZeroInPeriod (periodo - 1)
    |otherwise = True
--c)
periodo::Int
periodo = 7

zeros::Int->[Int]
zeros 0 = []
zeros dia
    |vendas dia == 0 = dia : (zeros (dia-1))
    |otherwise = zeros (dia-1)

zerosInPeriod::[Int] --Interface
zerosInPeriod = zeros periodo

--d)
vendasBaixas::Int->Int->[Int]
vendasBaixas _ 0 = []
vendasBaixas quant dia
    |quant > vendas dia = dia : (vendasBaixas quant (dia-1))
    |otherwise = vendasBaixas quant (dia-1)

listaVendasBaixas::Int->[Int]
listaVendasBaixas quant = vendasBaixas quant periodo

{-15)A sequencia de Fibonacci é definida e conhecida na literatura. Os dois primeiros números são 0 e 1, e os seguintes são 
calculados como a soma dos dois anteriores na sequência. Defina a função antFib que, dado um valor x, calcule a posição de x na 
sequencia de Fibonacci. Caso x não esteja na sequência, retorne (-1)-}
fibonacci::Int->Int
fibonacci   0 = 0
fibonacci   1 = 1
fibonacci   x = fibonacci (x-1) + fibonacci (x-2)

findFibonacci::Int->Int->Int
findFibonacci value  n
            | fibonacci n < value = findFibonacci value (n+1)
            | fibonacci n == value = n
            | otherwise = -1

antFib::Int->Int
antFib value = findFibonacci value 0

{- 16. Escreva uma definição equivalente à exibida abaixo, mas usando apenas uma única cláusula
em casamento de padrão: 
      funny x y z
          | x > z = True
          | y >= x = False
          | otherwise = True 
-}
funny :: Int -> Int -> Int -> Bool --não sei se está certo
funny x y z
    |y >= x = False
    |otherwise = True


{- 17. Implemente uma função que converte uma letra minúsculas como entrada para seu equivalente em maiúsculo. Caso a entrada não 
seja uma letra minúscula, retorne o próprio caractere de entrada. Como dica, veja a função predefinida isLower::Char->Bool. Para 
verificar outras funções pré-definidas para o tipo Char, consulte a biblioteca padrão no endereço 
http://zvon.org/other/haskell/Outputglobal/index.html. -}
paraMaius :: Char -> Char
paraMaius letra
    |isLower letra =  chr (ord (letra) - 32) --toUpper letra / Se é minuscula é verdadeira
    |otherwise = letra

{- 18. Defina uma função charToNum::Char->Int que converte um dígito numérico do tipo Char (como '3') para o valor que ele 
representa em Int, (3). Se o caractere de entrada não representa um dígito numérico, a função deve retornar -1. Como dica, veja as 
funções isDigit, chr e ord do módulo Data.Char. -}
charToNum::Char->Int 
charToNum letra
    |isDigit letra = ord letra - ord '0'
    |otherwise = (-1)

{- 19. Implemente a função duplicate::String->Int->String que recebe uma string s e um número inteiro n. A função deve retornar a 
concatenação de n cópias de s. Se n for zero, retorna "". Como dica, usar o operador de concatenação pré-definido 
(++)::String->String->String. -}
duplicate::String->Int->String
duplicate s n
    |n > 0 = s ++ (duplicate s (n-1))
    |otherwise = ""

{- 20. Implemente a função pushRight::String->Int->String que recebe uma string s e um número inteiro n e retorna uma nova string 
t com k caracteres '>' inseridos no início de s. O valor de k deve ser tal que o comprimento de t seja igual a n. Obs: se n é menor 
que o comprimento de s, a função retorna a própria string s. -}
pushRight::String->Int->String
pushRight s n
    |n > length s = ">" ++ pushRight s (n-1)
    |otherwise = s

--22)Faça em Haskell uma solução para inverter os elementos de uma lista de Inteiros
inverte :: [a] -> [a]
inverte [] = []
inverte (b:bs) = (inverte bs) ++ [b]

{- 23. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de inteiros onde a primeira 
conterá os elementos ímpares e a segunda os elementos pares passados como parâmetro. -}
separa :: [Int] -> ([Int],[Int])
separa x = (listaImpar x , listaPar x)

listaPar :: [Int] -> [Int]
listaPar [] = []
listaPar (b:bs)
    |mod b 2 == 0 = b : listaPar bs
    |otherwise = listaPar bs

listaImpar :: [Int] -> [Int]
listaImpar [] = []
listaImpar (b:bs)
    |mod b 2 /= 0 = b : listaImpar bs
    |otherwise = listaImpar bs


{- 24. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar a string contendo as letras do alfabeto cuja posição 
é dada pelos elementos da lista. -}
converte :: [Int] -> String
converte [ ] = ""
converte (a:as) = (chr (64 + a)) : converte as


--25)Sabendo que [1..7] é equivalente à lista [1,2,3,4,5,6,7], complete as correspondências abaixo:
--(a) ['a'..'g'] = "abcdefg"
--(b) [0.1 ..0.9] = [0.1,1.1]
--(c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
--(d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.9,1.0,1.3,1.5,1.7,1.9]
--(e) [0.4,0.2 ..0.8] = []
--(f) [1,4..15] = [1,4,7,10,13]


{- 26. Faça em Haskell uma solução para o seguinte problema: Dada uma lista de caracteres [Char],
e um caractere a, retornar quantos caracteres da lista são iguais a a. -}
conta :: [Char] -> Char ->Int
conta [] _ = 0
conta (a:as) b
    |a == b = 1 + conta as b
    |otherwise = conta as b

{- 27. Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista
de inteiros ordenada sem elementos repetidos. -}
purifica :: [Int] -> [Int]
purifica [] = []
purifica (as:[]) = [as]
purifica (a:b:as)
    |a == b  = purifica (b:as)
    |otherwise = a : purifica (b:as)

{- 28. Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista com uma
repetição de cada elemento de acordo com seu valor. -}
repete :: Int -> Int -> [Int]
repete num vezes
    |vezes > 0 = num : repete num (vezes-1)
    |otherwise = []

proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (a : as) = (repete (a) (a)) ++ proliferaInt as


{- 29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto. -}
repeteChar :: Char -> Int -> [Char]
repeteChar letra vezes
    |vezes > 0 = letra : repeteChar letra (vezes-1)
    |otherwise = []


proliferaChar :: [Char] -> [Char]
proliferaChar [] = []
proliferaChar (a:as) = (repeteChar a ((ord a)-64)  ) ++ proliferaChar as
