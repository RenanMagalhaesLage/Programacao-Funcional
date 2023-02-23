import Data.Char

{- 1. Defina uma função que retorne uma tupla-3 (tripla) contendo o caractere fornecido com entrada, o mesmo caractere em letras 
minúsculas ou maiúsculas, e o seu número da tabela ASCII. Exemplo: -}
paraMaius :: Char -> Char
paraMaius letra
    |isLower letra =  chr (ord (letra) - 32) --toUpper letra / Se é minuscula é verdadeira
    |otherwise = letra

converte :: Char -> (Char, Char, Int)
converte letra
    |letra == paraMaius letra = (letra, chr (ord (letra) + 32), ord letra ) --chr :: int char       ord :: char int
    |otherwise = (letra, chr (ord (letra) - 32), ord letra )

{- 2. Seja o cadastro de pessoas dado pela função a seguir. Construa funções que retornem os seguintes dados: -}

--a)O nome da pessoa de menor idade até um determinado registro
pessoa :: Int -> (String, Int, Char)
pessoa rg 
    |rg == 1 = ("Joao Silva", 12, 'm')
    |rg == 2 = ("Jonas Souza", 51, 'm')
    |rg == 3 = ("Doja Cat", 25, 'f')
    |rg == 4 = ("Katy Perry", 38, 'f')
    |rg == 5 = ("Marcos Junior", 87, 'm')
    |rg == 6 = ("Avril Lavige", 40, 'f')
    |rg == 7 = ("Lucas Vasconcelos", 18, 'm')
    |rg == 8 = ("Maria Clara", 26, 'f')
    |rg == 9 = ("Sky Ferreira", 29, 'f')
    |rg == 10 = ("Orlando Bloom", 43, 'm')
--    |rg == 321 = ("Jocileide Strauss" , 21, 'f')
    |otherwise = ("Não há ninguém mais", 9999, 'x')

idade :: (String, Int, Char) -> Int
idade (_,b,_) = b

nome :: (String, Int, Char) -> String
nome (a,_,_) = a

sexo :: (String, Int, Char) -> Char
sexo (_,_,c) = c

--Função que retorna a menor Idade
menorIdade :: Int -> Int -> Int -- Parametros: registro idade(pessoa registro)
menorIdade 0 menor = menor
menorIdade registroLimite menor
    |idade (pessoa registroLimite) < menor = menorIdade (registroLimite-1) (idade (pessoa registroLimite))
    |otherwise = menorIdade (registroLimite-1) menor

--Função que retorna o nome do mais novo
nomeMenorIdade :: Int -> Int -> String 
--Recebe o valor da menor idade e o registro limite e devolve o nome da menor idade
nomeMenorIdade menor registro
    |idade(pessoa registro) == menor = nome(pessoa registro)
    |otherwise = nomeMenorIdade menor (registro-1) 
--EXEMPLO: nomeMenorIdade (menorIdade 10 (idade(pessoa 10))) 10

--b)A idade média de todas as pessoas até um dado registro
type Registro = Int

--Função para calcular a soma de todas as idade até dado registro
totalIdade :: Registro -> Int
totalIdade 0 = 0
totalIdade registro = idade(pessoa registro) + totalIdade (registro - 1)

idadeMedia :: Registro -> Int
idadeMedia registro = div (totalIdade registro) registro

--c)O número de pessoas do sexo masculino
homem :: Registro -> Int
homem 0 = 0
homem registro
    |sexo(pessoa registro) == 'm' = 1 + homem (registro - 1)
    |otherwise = homem (registro - 1)

--d)O número do registro da pessoa de maior idade
maiorIdade :: Int -> Int -> Int -- Parametros: registro idade(pessoa registro)
maiorIdade 0 maior = maior
maiorIdade registroLimite maior
    |idade (pessoa registroLimite) > maior = maiorIdade (registroLimite-1) (idade (pessoa registroLimite))
    |otherwise = maiorIdade (registroLimite-1) maior

registroMaiorIdade :: Int -> Int -> Int
--Recebe o valor da maior idade e o registro limite e devolve o registro
registroMaiorIdade _ 0 = 0
registroMaiorIdade maior registrolimite
    |idade(pessoa registrolimite) == maior = registrolimite
    |otherwise = registroMaiorIdade maior (registrolimite-1) 

--3) --> Igual a número 1

--4)Construa uma função em Haskell que recebe 4 inteiros e devolve uma tupla-4 com os quatro valores originais, só que ordenados
--menorNum :: Int -> Int -> Int ->Int -> [Int]
--menorNum x y
--    |x >= y = y
--    |otherwise = x

--ordena :: Int -> Int -> Int -> Int ->(Int,Int,Int,Int)
--ordena x y z w = converteTupla(menorNum x y z w)
 


{- 5. Dadas duas datas (dI, mI, ar) e (d2, m2, a2), tal que data, ::; data-, construa uma função que retorne quantos dias existem 
entre estas duas datas, onde di define o dia do mês mj no ano ak.-}
--datas :: (Int, Int, Int) -> (Int, Int, Int) -> Int
--datas 0 (d2,m2,a2) = (d2,m2,a2) 
--datas (d1,m1,a1) 0 = (d1,m1,a1)
--datas (d1,m1,a1) (d2,m2,a2)
    
{- 6. Crie uma função que receba os coeficientes de uma equação do segundo grau ax^2 + bx + c = O na forma (a, b, c) e retome as 
raízes desta equação. Trate o caso de raízes imaginárias, indicando um erro. -}
delta :: (Double, Double, Double) -> Double
delta (a,b,c) = (b*b) - (4*a*c)

equacao :: (Double, Double, Double) -> (Double,Double)
equacao (a,b,c)
    |delta (a,b,c) > 0 = (((-b) + (sqrt (delta (a,b,c)))) / (2*a), ((-b) - (sqrt (delta (a,b,c)))) / (2*a))
    |otherwise = ((-1),(-1))

{- 7. Construa uma função que, dados três valores, verifique se os mesmos podem ser os lados de um triângulo. Se for possível 
formar o triângulo, retome uma tupla-2 com o tipo do triângulo formado (com relação às arestas) e o perímetro do mesmo. -}
dif :: Int -> Int -> Int
dif x y 
    | x - y < 0 = -1 * (x - y)
    |otherwise = (x - y)

ehTri :: (Int, Int, Int) -> Bool  --Função para calcular se é triangulo
ehTri (a,b,c)
    |(dif b c) < a && a < b + c = True
    |(dif a c) < b && b < a + c = True
    |(dif a b) < c && c < a + b = True
    |otherwise = False

tipoTri :: (Int,Int,Int) -> String --Função para falar qual tipo de triangulo é
tipoTri (a,b,c)
    |a==b && a==c = "Equilatero"
    |a /= b && a /= c && b/=c = "Escaleno"
    |a == b || a == c || b == c = "Isosceles"
    |otherwise = "Nao eh triangulo"

triangulo :: (Int,Int,Int) -> (String, Int)
triangulo (a,b,c)
    |ehTri (a,b,c) = (tipoTri (a,b,c), (a+b+c))
    |otherwise = (tipoTri (a,b,c), 0)


--8)Apresentada uma base de dados de 10 professores,construa funções que retomem:


base :: Int -> (Int, String, String, Char)
base x
    |x == 0 = (1793, "Pedro Paulo", "MESTRE", 'M')
    |x == 1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
    |x == 2 = (1534, "Joao de Medeiros", "DOUTOR", 'M')
    |x == 3 = (1267, "Claudio Cesar de Sa", "DOUTOR", 'M')
    |x == 4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
    |x == 5 = (1888, "Rita de Matos", "MESTRE", 'F')
    |x == 9 = (1968, "Tereza Cristina Andrade", "MESTRE", 'F')
    |otherwise = (0, "", "", 'O')

--a)O número de doutores na base.
matricula :: (Int, String, String, Char) -> Int
matricula (a,b,c,d) = a

nome1 :: (Int, String, String, Char) -> String
nome1 (a,b,c,d) = b

cargo :: (Int, String, String, Char) -> String
cargo (a,b,c,d) = c

sexo1 :: (Int, String, String, Char) -> Char
sexo1 (a,b,c,d) = d


type Quant = Int
numDoutor :: Quant -> Int
numDoutor quant
    |quant < 0 = 0
    |(cargo(base quant)) == "DOUTOR" = 1 + numDoutor (quant - 1)
    |otherwise = numDoutor (quant - 1)


--b) O número de mulheres.
numMulher :: Quant -> Int
numMulher quant
    |quant < 0 = 0
    |(sexo1(base quant)) == 'F' = 1 + numMulher(quant-1)
    |otherwise = numMulher(quant-1)

--c)O número de mestres do sexo masculino
mestreHomem :: Quant -> Int
mestreHomem quant
    |quant < 0 = 0
    |(sexo1(base quant)) == 'M' && (cargo(base quant)) == "MESTRE" = 1 + mestreHomem(quant -1)
    |otherwise = mestreHomem(quant -1)

--d)O nome do professor mais antigo (número de menor matrícula)

menorMatricula :: Quant -> Int ->Int
menorMatricula quant menorM
    |quant < 0 = menorM
    |(matricula(base quant)) < menorM && (matricula(base quant)) /= 0 = (menorMatricula (quant-1) (matricula(base quant)))
    |otherwise = menorMatricula (quant-1) (menorM)

nomeProfAntigo :: Quant -> String 
nomeProfAntigo quant
    |quant < 0 = "ERRO"
    |matricula(base quant) == (menorMatricula (9) (matricula(base 9))) = nome1(base quant)
    |otherwise = nomeProfAntigo (quant - 1)