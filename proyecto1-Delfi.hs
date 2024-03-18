{-# LANGUAGE NPlusKPatterns #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
-- 1.
--a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.

esCero :: Int -> Bool
esCero x = x == 0

{-
EJEMPLOS:
esCero (-1) -> False
esCero (-13) -> False
esCero 0 -> True
esCero 2 -> False
-}

--b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo x = x > 0 

{-
EJEMPLOS:
esPositivo 1 -> True
esPositivo (-1) -> False
esPositivo 0 -> False
-}

--c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula.
esVocal :: Char -> Bool
--esVocal x = (x == 'a') || (x == 'e') || (x == 'i') || (x == 'o') || (x == 'u')
esVocal x = elem x "aeiou"
-- si tambien pidiera las vocales en mmayuscula "AEIOU" deberia usar || 

{-
EJEMPLOS:
 esVocal 'D'
False
 esVocal 'e'
True
 esVocal 'l'
False
 esVocal 'f'
False
 esVocal 'I'
False
-}

--d) valorAbsoluto :: Int -> Int, que devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto x | x <0 = x*(-1) 
                | x >=0 =x

--2. Programa las siguientes funciones usando recursion o composicion:
--a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True.
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x : xs) = x && paratodo xs

{-
EJEMPLOS:
paratodo [True,True] -> True
paratodo [True] -> True
paratodo [False,True,False] -> False
paratodo [True,False] -> False
-}

--b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una lista de enteros.

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{-
EJEMPLOS:
 sumatoria [1,2,3]
6
 sumatoria [1,1,1,1]
4
-}

--c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

{-
EJEMPLOS:
productoria [1,2,4]
8
productoria []     
1
productoria [2,0,5]
0
-}

--d) factorial :: Int -> Int, que toma un numero n y calcula n!.(si n=3 entonces 3*2*1)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{-
EJEMPLOS:
 factorial 3 -> 6
 factorial 0 -> 1
 factorial 1 -> 1
 factorial 5 -> 120
-}

--e) Utiliza la funcion sumatoria para definir, promedio :: [Int] -> Int, 
--toma una lista de numeros no vacıa y calcula el valor promedio (truncado, usando division entera).

promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)   

{-
EJEMPLOS:
 promedio [7,7,7]
7
 promedio []     
0
 promedio [9,10,3,7]
7
-}

--3. Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se encuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x : xs)
  | n == x = True
-- | pertenece n xs = n `elem` xs
  | otherwise = pertenece n xs

{-
EJEMPLOS:
 pertenece 0 [8,2,0,1]
True
 pertenece 4 [1,2,3,5]
False
-}

--4. Programa las siguientes funciones que implementan los cuantificadores generales.
--(el segundo parametro de cada funcion, es otra funcion)

--a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado T :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado T.

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x : xs) t = t x && paratodo' xs t

{-
EJEMPLOS:
 paratodo' [] esCero
True
 paratodo' [0,1,2] esCero
False
 paratodo' [0,1,2] esPositivo
False
 paratodo' [1,2] esPositivo  
True
-}

--b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado T :: a -> Bool, determina si algun elemento de xs satisface el predicado T.

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] funcion = False
existe' (x : xs) funcion
  | funcion x = True
  | otherwise = existe' xs funcion

{-
EJEMPLOS:
 existe' [0] esCero      
True
 existe' [] esCero             
False
 existe' [2,(-3)] esPositivo
True
 existe' [(-2),(-3)] esPositivo
False
-}

--c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una funcion T :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicación de T a los elementos de xs.

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x : xs) t = t x + sumatoria' xs t

{-
EJEMPLOS: (sumatoria' [1,2,3,4,5] (+2)  y el (+2) es la funcion A -> Int, 
           sumatoria' [1,2,3](+2)  procedimiento: (1+2)+(2+2)+(3+2)= 3+4+5 = 12)
 sumatoria' [1,2,3](+1) 
9
 sumatoria' [1,2,3,5] (+1) 
15
 sumatoria' [1,2,3,5] (*2)   
22
-}

--d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una funcion T :: a -> Int, calcula el producto de los valores que resultan de la aplicacion de T a los elementos de xs.

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x : xs) t = t x * productoria' xs t

{-
EJEMPLOS:
 productoria' [1,2,3,5] (*2) 
480
 productoria' [1,2,3,5] (*1) 
30
 productoria' [1,2] (*1)     
2
 productoria' [1,2] (+1) 
6
-}

--5. Definir nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo’ (sin recursion ni analisis por casos).

paratodo'' :: [Bool] -> Bool 
paratodo'' xs = paratodo' xs id

{-
EJEMPLOS:
 paratodo'' [True,True] 
True
 paratodo'' [True,False]
False
-} 

--6. Utilizando las funciones del ejercicio 4, programa las siguientes funciones por composicion, sin usar recursion ni analisis por casos.

--a) todosPares :: [Int] -> Bool verifica que todos los números de una lista sean pares. 
-- even= comprueva si un numero es par / podriamos hacer funcion esPar

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even 

{-
EJEMPLOS:
 todosPares [2,4,6,8] 
True
 todosPares [2,4,6,7]
False
-}

--b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algún número dentro del segundo parámetro que sea múltiplo del primer parámetro.

multiplo :: Int -> Int -> Bool
multiplo n z = mod z n == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (multiplo n)

{-
EJEMPLOS:
 hayMultiplo 2 [1,3,4,5,6]
True  
 hayMultiplo 2 []         
False 
-}

--c) sumaCuadrados :: Int -> Int, dado un número no negativo n, calcula la suma de los primeros n cuadrados, i : 0 ≤ i < n : i
--Ayuda: En Haskell se puede escribir la lista que contiene el rango de números entre n y m como [n..m].
--cuacrado usa sumatoria' y para hacer los numeros hasta M usa [0...M] luego le aplicas la funcion (^2)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..(n-1)] (^ 2)

{-
EJEMPLOS:
 sumaCuadrados 5 
30
 sumaCuadrados 0 
0
 sumaCuadrados 20
2470
-}

--d) Programar la fucion existeDivisor::Int-> [Int] -> Bool, que dado en entero n y una lista ls, devuelve True si y solo si, existe algún elemento en ls que divida a n.

divisor:: Int -> Int -> Bool
divisor a b = a `mod` b == 0

existeDivisor :: Int -> [Int] -> Bool
--existeDivisor n [] = False
existeDivisor n ls = existe' ls (divisor n)

{-
EJEMPLOS:
 existeDivisor 5 [10,5,35]
True
 existeDivisor 5 []       
False
 existeDivisor 2 [30,22,1]
True
-}

--e) Utilizando la funcion del apartado anterior, definı la función esPrimo:: Int -> Bool, que dado un entero n, devuelve True si y solo si n es primo.
--Los números primos son aquellos que solo son divisibles entre ellos mismos y el 1, como 1 no es primo empezamos a utilizar desde el numero 2 

esPrimo :: Int -> Bool
esPrimo n = not (existeDivisor n [2..(n-1)]) && (n/=1)--1 es el unico q satisfase existeDivisor 

{-
EJEMPLOS:
 esPrimo 1
False
 esPrimo 11
True
 esPrimo 2
True
 esPrimo 3
True
 esPrimo 10
False
-}

--f ) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursion? 
{-2.d) un numero factorial es la multiplicacion de ese numero hasta 1 entonces buscamos una funcion q haga la mutiplicacion de 1 hasta ese numero n
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)-} 

factorial'' :: Int -> Int 
factorial'' n = productoria [1..n]

{-
EJEMPLOS:
ghci> factorial'' 5
120
ghci> factorial'' 3
6
ghci> factorial'' 2
2
-}

--g) Programar la función multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los números primos de una lista.

primo :: Int -> Int
primo x | esPrimo x = x
        | otherwise = 1 --porque no afecta a la multiplicacion de multiplicaPrimos 

multiplicaPrimos :: [Int] -> Int 
multiplicaPrimos xs = productoria' xs primo

{-
EJEMPLOS:
 multiplicaPrimos [1,2,3]
6
 multiplicaPrimos []
1
 multiplicaPrimos [2,5,1]
10
-}

--h) Programar la función esFib :: Int -> Bool, que dado un entero n, devuelve True si y solo si n esta en la sucesion de Fibonacci.
--Ayuda: Realizar una función auxiliar fib :: Int -> Int que dado un n devuelva el n-esimo elemento de la sucesion.

--serie infinita de números naturales que empieza con un 0 y un 1 y continúa añadiendo números que son la suma de los dos anteriores: 0, 1, 1, 2, 3, 5, 8, 13, 21...

fib :: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = fib (n - 1) + fib (n - 2)

listafib :: Int -> [Int]
--necesita agregar los dos primeros elementos de la sucesion 0,1 cuando llegue a 1
listafib 0 = [0..1]
listafib n = listafib (n - 1) ++ [fib n]
--si n pertenece en fib y existe en listafib devuelve True en esFib
esFib :: Int -> Bool 
esFib n = existe' (listafib n) (== n)

{-
EJEMPLOS:
listafib 2 --> [0,1,1,1]
listafib 13 --> [0,1,1,1,2,3,5,8,13,21,34,55,89,144,233]
listafib 5 --> [0,1,1,1,2,3,5]
esFib 5 --> True
esFib 0 --> True
esFib 4 --> False
esFib 10 --> False
-}

--i) Utilizando la función del apartado anterior, definı la función todosFib :: [Int] -> Bool que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen (o no) a la sucesión de Fibonacci.
todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib 

--7. Indaga en Google sobre las funciones map y filter. Tambien podes consultar su tipo en ghci con el comando :t.
-- ¿Que hacen estas funciones?¿A que equivale la expresión map succ [1, -4, 6, 2, -8], donde succ n = n+1?¿Y la expresión filter esPositivo [1, -4, 6, 2, -8]?
{-
la función map aplica la funcion a cada elemento de la lista y devuelve otra lista con los resultados, es decir q se obtiene una lista (map f xs) aplicando f en cada elemento de xs
en haskell se la define como:
map : : ( a -> b) -> [a] -> [b]
la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1 está sumando +1 a cada elemento de la lista, por lo q devuelve la lista [2, -3, 7, 3,-7]
la función filter, (filter P xs) es la lista de los elementos de xs q cumplen con la propiedad P
se la define:
filter : : (a -> Bool) -> [a] -> [a]
la expresion filter esPositivo [1, -4, 6, 2, -8] filtra entre enteros positivos y negativos dejando solo los positivos siendo su resultado la lista [1,6, 2]
-}

--8. Programa una funcion que dada una lista de números xs, devuelve la lista que resulta de duplicar cada valor de xs.
--a) Definila usando recursion.

duplicarValor :: [Int] -> [Int]
duplicarValor  [] = []
duplicarValor (x:xs) = (x*2): duplicarValor xs

{-
EJEMPLOS:

-}

--b) Definila utilizando la funcion map.
duplicaValor' :: [Int] -> [Int]
duplicaValor' xs = map (*2) xs

{-
EJEMPLOS:

-}

--9. Programa una funcion que dada una lista de números xs, calcula una lista que tiene como elementos aquellos números de xs que son primos.
--a) Definila usando recursion.

listPrimo:: [Int] -> [Int]
listPrimo [] = []
listPrimo (x:xs) | esPrimo x == True = x: listPrimo xs
                 | esPrimo x == False = listPrimo xs

{-
EJEMPLOS:

-}

--b) Definila utilizando la función filter.

listPrimo' :: [Int] -> [Int]
listPrimo' xs = filter esPrimo xs

{-
EJEMPLOS:

-}

--c) Revisa tu definicion del ejercicio 6g. ¿Se puede mejorar?

{-- 6.g)
multiplicaPrimos :: [Int] -> Int 
multiplicaPrimos xs = productoria' xs primo
-} 

multiplicaPrimos' :: [Int] -> Int 
multiplicaPrimos' (x:xs) = primo x * multiplicaPrimos' xs 

{-
EJEMPLOS:

-}
--10. La funcion primIgualesA toma un valor y una lista, y calcula el tramo inicial mas largo de la lista cuyos elementos son iguales a ese valor.
--a) Programa primIgualesA por recursión.

primIgualesA :: Eq a=> a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | n == x = x : primIgualesA n xs
                      | otherwise = []
{-
EJEMPLOS:

-}

--b) Programa nuevamente la función utilizando takeWhile.

--primIgualesA' :: Eq a => a -> [a] -> [a]
--primIgualesA' n xs =

--11. La funcion primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre sı.
--a) Programá primIguales por recursion.

primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:xs) | (x == head xs) = x : primIguales xs
                   | otherwise = [x]
               
{-
EJEMPLOS:

-}

--b) Usá cualquier versión de primIgualesA para programar primIguales. Esta permitido dividir en casos, pero no usar recursion.


