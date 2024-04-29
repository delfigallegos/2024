-- 2) Ejercicios:
-- 1. Tipos Enumerados
-- a)Implementá el tipo de Carrera como está definido.


data Carrera = Matematica | Fisica | Computación | Astronomia

-- b) Definí la sig funcion usando pattern matching : titulo::Carrera-> String que devuelve el nombre completo de la carrera en forma de string.
-- Ejemplo:
-- Matematica devuelve "Licenciatura en Matetmática"
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computación = "Licenciatura en Computación"
titulo Astronomia = "Licenciatura en Astronomia"

-- c) Para escribir musica se utiliza la denominada notacion musical, la cual consta de notas (do,re,mi, ...). Además estas notas pueden presentar algún modificador como sostenido o bemol,definir el tipo NotaBasica cons constructores Do,Re, Mi, Fa, Sol, La y Si.
data NotaBasica = Do| Re| Mi| Fa| Sol| La| Si deriving (Eq, Ord, Show, Bounded, Enum)

-- d) EL sistemade notacion musical anglosajon, tambien conocido como notacion o cifrado americado, relaciona las notas basicas con letras de la A a la G. Este sistema se usa por ejemplo para las tablaturas de guitarra.
-- Programar usando Pattern Matching la funcion: cifradoAmericano :: NotaBasica -> Char
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

-- cifradoAmericano Do = 'A'
-- cifradoAmericano Re = 'D'

-- 3 . Polimorfismo AD HOC Recordemos la función sumatoria del proyecto anterior:

-- a) Definir usando polimorfismo ad hoc la función minimoElemento que calcula (de manera recursiva) cúal es el menor valor de una lista de tipo [a]. Asegurarse que sólo esté definida para listas no vacías.

minimoElemento :: (Ord a) => [a] -> a
minimoElemento [x] = x
minimoElemento (x : xs) = min x (minimoElemento xs)

-- minimoElemento [-2,1,-3,-4] = -4
-- minimoElemento [-1,0,1] = -1

-- b) Definir la función minimoElemento' de manera tal que el caso base de la recursión sea el de la lista vacía. Para ello revisar la clase Bounded.
-- Ayuda: Para probar esta función dentro de ghci con listas vacías, indicar el tipo concreto con tipos de la clase Bounded, por ejemplo: ([1,5,10]::[Int]), ([]::[Bool]), etc.

minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x : xs) = min x (minimoElemento' xs)

-- minimoElemento' [3,1,3]::Int = 1
-- minimoElemento' ([1,5,10]::[Int]) = 1

-- C) Usar la función minimoElemento para determinar la nota más grave de la melodía:
-- [Fa, La, Sol, Re, Fa]

minimoElemento'' :: [NotaBasica] -> NotaBasica
minimoElemento'' x = minimoElemento x

-- minimoElemento'' [Fa, La, Sol, Re, Fa] = Re

-- 4 Sinonimo de tipos, constructores con parámetros

-- A) Implementar tipo Deportista

type Altura = Int -- Sinonimo de Tipo

type NumCamiseta = Int -- Sinonimo de Tipo

data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Show,Eq) -- Tipos algebráicos sin parámetros ( aka enumerados)

data TipoReves = DosManos | UnaMano deriving (Show) -- Tipos algebráicos sin parámetros ( aka enumerados)

data Modalidad = Carretera | Pista | Monte | BMX deriving (Show) -- Tipos algebráicos sin parámetros ( aka enumerados)

data PiernaHabil = Izquierda | Derecha deriving (Show) -- Tipos algebráicos sin parámetros ( aka enumerados)

type ManoHabil = PiernaHabil -- Sinónimo

-- Deportista es un tipo algebráico con constructores parámetricos
data Deportista
  = Ajedrecista -- Constructor sin argumentos
  | Ciclista Modalidad -- Constructor con un argumento
  | Velocista Altura -- Constructor con un argumento
  | Tenista TipoReves ManoHabil Altura -- Constructor con tres argumentos
  | Futbolista Zona NumCamiseta PiernaHabil Altura
  deriving (Show) -- Constructor con cuatro argumentos

-- B)¿Cúal es el tipo del constructor Ciclista?
--  El constructor Ciclista es de tipo Modalidad -> Deportista

-- c) Programá la función contar_velocistas :: [Deportista] -> Int que dada una lista de deportistas xs, devuelve la cantidad de velocistas que hay dentro de xs.
-- Programar contar_velocistas sin usar igualdad, utilizando pattern matching.

contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (Velocista _ : xs) = 1 + contar_velocistas xs
contar_velocistas (_ : xs) = contar_velocistas xs

-- contar_velocistas [(Ciclista Pista),(Velocista 2)] = 1
-- contar_velocistas [(Velocista 1),(Velocista 2)] = 2

-- d) Programá la función contar_futbolistas :: [Deportista] -> Zona -> Int que dada una lista de deportistas xs, y una zona z, devuelve la cantidad de futbolistas incluidos en xs que juegan en la zona z. No usar igualdad, sólo pattern matching.

contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] zona = 0
contar_futbolistas ((Futbolista Arco _ _ _) : xs) Arco = 1 + contar_futbolistas xs Arco
contar_futbolistas ((Futbolista Defensa _ _ _) : xs) Defensa = 1 + contar_futbolistas xs Defensa
contar_futbolistas ((Futbolista Mediocampo _ _ _) : xs) Mediocampo = 1 + contar_futbolistas xs Mediocampo
contar_futbolistas ((Futbolista Delantera _ _ _) : xs) Delantera = 1 + contar_futbolistas xs Delantera
contar_futbolistas (_ : xs) zona = contar_futbolistas xs zona

-- contar_futbolistas (x : xs) zona = case x of
--   Futbolista zona _ _ _ -> 1 + contar_futbolistas xs zona
--   _ -> 0 + contar_futbolistas xs zona

-- contar_futbolistas  [(Ciclista Pista),Futbolista Arco 1 Derecha 10] Arco = 1
-- contar_futbolistas  [(Ciclista Pista),Velocista 2] Arco = 0

-- e) ¿La función anterior usa filter? Si no es así, reprogramala para usarla.

esFutbolista :: Zona -> Deportista -> Bool
esFutbolista zonaBusco (Futbolista zonaFutbol _ _ _) = zonaFutbol == zonaBusco 
esFutbolista zonaBusco _ = False 


contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' [] zona = 0
contar_futbolistas' xs zona = length(filter (esFutbolista zona) xs)

--5) Definicion de clases
--a) 
sonidoNatural :: NotaBasica -> Int 
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

--b) 
data Alteracion = Bemol | Natural | Sostenido deriving (Eq, Ord) 

--c)
data NotaMusical = Nota (NotaBasica, Alteracion) deriving (Eq, Ord) 

--d)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico ( Nota (notaBasica, Sostenido)) = sonidoNatural(notaBasica) + 1  
sonidoCromatico ( Nota (notaBasica, Bemol))     = sonidoNatural(notaBasica) - 1  
sonidoCromatico ( Nota (notaBasica, Natural))   = sonidoNatural(notaBasica)  

--e) ejemplo: (Nota(Do,Sostenido))==(Nota(Do,Sostenido)) -> True

{-
f) ejemplo: (Nota(Do,Sostenido))<=(Nota(Do,Sostenido)) -> True
 (Nota(Do,Sostenido))<(Nota(Do,Sostenido))  -> False 
-}

--6) Tipos enumerados con polimorfismo
primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing 
primerElemento (x:xs) = Just x 

{-
 primerElemento []  ->Nothing
 primerElemento [4,7,2] ->Just 4
-}

--7) Tipos recursivos 
data Cola = VaciaC | Encolada Deportista Cola deriving (Show) -- Deportista = x  y Cola = xs Deportista Cola = (x:xs)

--a.1) Elimina de la cola a la primera persona ya atendida
atender :: Cola -> Maybe Cola 
atender VaciaC= Nothing
atender (Encolada deportista cola) = Just cola

{- 
atender (Encolada Ajedrecista (Encolada (Ciclista BMX) (VaciaC))) -> Just (Encolada (Ciclista BMX) VaciaC)
-}

--a.2) Agrega una persona en la ultima posicion a la cola de deportistas
encolar :: Deportista -> Cola -> Cola 
encolar deportistaAgregar VaciaC= Encolada (deportistaAgregar) VaciaC
encolar deportistaAgregar (Encolada (deportista) cola) = case cola of
  Encolada _ VaciaC -> Encolada (deportista) (Encolada deportistaAgregar (VaciaC))
  _-> encolar deportistaAgregar cola

{-
encolar Ajedrecista (Encolada Ajedrecista (VaciaC)) --> Encolada Ajedrecista (Encolada Ajedrecista VaciaC)
encolar (Ciclista BMX) (Encolada Ajedrecista (Encolada Ajedrecista VaciaC)) -> Encolada Ajedrecista (Encolada (Ciclista BMX) VaciaC)
encolar (Ciclista BMX) (Encolada Ajedrecista (VaciaC)) --> Encolada Ajedrecista (Encolada (Ciclista BMX) VaciaC)
 -}

--a.3) Devuelve el/la primera futbolista dentro de la cola que juega en la zona que se corresponde con el segundo parametro. Si no hay futbolistas jugando en esa zona devuelve Nothing
 
busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing 
busca (Encolada deportista cola) zona = case deportista of
  Futbolista zona _ _ _ -> Just deportista
  _ -> busca cola zona

--b) ¿A q otro tipo se parece cola? Se parece al tipo deportista 

--8) Tipos recursiovos y polimorficos 
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

--a) Guia telefonica
type GuiaTelefonica = ListaAsoc String Int

--b.1) Devuelve la cantidad de datos en una lista

la_long :: ListaAsoc a b -> Int 
la_long Vacia = 0
la_long (Nodo _ _ lista)= 1 + la_long lista

--b.2) Devuelve la concatenacion de dos listas
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b 
la_concat (Vacia) (Vacia) = Vacia
la_concat (Vacia) (Nodo x z lista2) = Nodo x z (lista2) 
la_concat (Nodo x z lista1) (Vacia) = Nodo x z (lista1)
la_concat (Nodo x z lista1) (Nodo y t lista2) = Nodo x z (Nodo y t (lista2))

{-
 la_concat (Nodo 'y' 'u' (Vacia)) (Nodo 't' 'u' (Vacia)) --> Nodo 'y' 'u' (Nodo 't' 'u' Vacia)
-}

--b.3) agrega un nodo a la lista de asociaciones si la clave no esta en la lista, si no actualiza el valor 

la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b 
la_agregar (Vacia) clave valor = Nodo clave valor (Vacia)
la_agregar (Nodo x y lista) clave valor | x==clave = Nodo x valor lista
                                        | otherwise = Nodo x y (la_agregar lista clave valor)

{-
la_agregar  (Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))) 'r' 6 --> Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 (Nodo 'r' 6 Vacia)))
la_agregar  (Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))) 'b' 6 --> Nodo 'a' 1 (Nodo 'b' 6 (Nodo 'c' 4 Vacia))
la_agregar  (Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))) 'a' 6 --> Nodo 'a' 6 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))
-}  

--b.4) Transforma una lista de asociaciones en una lista de pares clave-dato

la_pares :: ListaAsoc a b -> [(a , b)] -- Nodo 'a' 3(Nodo 'b' 4) -> [('a',3),('b',4)]
la_pares Vacia = [] 
la_pares (Nodo clave dato lista) = [(clave,dato)]++la_pares lista 

--b.5) dada una lista y una clave devuelve el dato asociado, si es q existe. Caso contrario devuelve Nothing

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo clave dato lista) claveBusco | clave == claveBusco = Just dato | otherwise = la_busca lista claveBusco

{-
la_busca (Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))) 'a' --> Just 1
la_busca (Nodo 'a' 1 (Nodo 'b' 3 (Nodo 'c' 4 Vacia))) 'b' --> Just 3
-}

--b.6) dada una clave a elimina la entrada en la lista 
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar clave Vacia = Vacia 
la_borrar clave (Nodo x y lista) | clave == x = lista
                                 | otherwise = (Nodo x y (la_borrar clave lista)) 

{-Ladrillos, Viguetas,  Cemento .  La idea es poder detallar para cada tipo de material, las características más importantes. En tal sentido identificamos las siguientes características de cada uno de los materiales a tener en cuenta:

Ladrillo
TipoLadrillo, que es un tipo enumerado con las siguientes opciones:  Ceramico, Hormigon, Tradicional
UsoDeLadrillo, que es un tipo enumerado con las siguientes opciones: Pared, Techo
Precio, que es un sinónimo de Int indicando el precio 

Vigueta
Largo, que es un sinónimo de Float indicando el largo de la vigueta
MaterialViga, que es un tipo enumerado con las siguientes opciones:  Hierro, Madera .
Precio, que es un sinónimo de Int indicando el precio  

Cemento
MarcaCemento, que es un tipo enumerado con las siguientes opciones:  Minetti, LomaNegra .
Precio, que es un sinónimo de Int indicando el precio 

Para ello: 

a) Definir el tipo MaterialesConstruccion que consta de los constructores Ladrillo, Vigueta y Cemento, constructores con parámetros descriptos arriba (Se deben definir también los tipos enumerados TipoLadrillo,UsoDeLadrillo, MaterialViga, MarcaCemento, y el sinónimo de tipos Precio). Los tipos MaterialesConstruccion y MaterialViga no deben estar en la clase Eq, ni en la clase Ord. Agregue la clase Show en los tipos que necesite.
-}

data TipoLadrillo = Ceramico | Hormigon | Tradicional deriving (Show)
data UsoDeLadrillo = Pared | Techo deriving (Show) 

type Precio = Int 
type Largo = Float 

data MaterialViga = Hierro | Madera deriving (Show)

data MarcaCemento = Minetti | LomaNegra deriving (Show) 

data MaterialesConstruccion = Ladrillo TipoLadrillo UsoDelLadrillo Precio | Vigueta Largo MaterialViga Precio | Cemento MarcaCemento Precio deriving (Show, Eq) 

--b) Definir la funcion ladrillosDeMenorPrecio, dada una lista de MaterialesConstruccion lm y un valor n de Precio,  devuelve la lista de MaterialesConstruccion que son Ladrillo en lm y que tienen un precio menor o igual a n. 

ladrillosDeMenorPrecio :: [MaterialesConstruccion] -> Int -> [MaterialesConstruccion]
ladrillosDemenorPrecio [] _ = []
ladrillosDeMenorPrecio (Ladrillo a b c : lm) precio | c<=precio = (Ladrillo a b c) : ladrillosDeMenorPrecio lm precio
                                                    | otherwise = ladrillosDeMenorPrecio lm precio 
ladrillosDeMenorPrecio (_:lm) precio = ladrillosDeMenorPrecio lm precio 

lista=[Cemento Minetti 10,Ladrillo Ceramico Pared 4,Ladrillo Hormigon Techo 5]

{--Ejemplos de ejecucion:
 
 -- ladrillosDeMenorPrecio lista 5
 -- [Ladrillo Ceramico Pared 4,Ladrillo Hormigon Techo 5]
 -- ladrillosDeMenorPrecio lista 4
 -- [Ladrillo Ceramico Pared 4]-}

--c) Definir igualdad para el tipo de MaterialesConstruccion: de tal manera que, 
--dos valores de tipo Ladrillo son iguales sólo si tienen el mismo tipo de ladrillo y el mismo precio, 
--dos Vigueta son iguales solo si tienen el mismo largo y el mismo materialViga,
--dos Cemento son iguales si tienen la misma marca . Como es de suponer Ladrillo, Vigueta y Cemento son distintos entre sí.
-- Puede crear funciones auxiliares si necesita.

mismoTipo :: TipoLadrillo -> TipoLadrillo -> Bool
mismoTipo Ceramico Ceramico = True
mismoTipo Hormigon Hormigon = True
mismoTipo Tradicional Tradicional = True
mismoTipo _ _ = False
 
mismoMaterial :: MaterialViga -> MaterialViga -> Bool
mismoMaterial Hierro Hierro = True
mismoMaterial Madera Madera = True
mismoMaterial _ _ = False
 
mismaMarca :: MarcaCemento -> MarcaCemento -> Bool
mismaMarca Minetti Minetti = True
mismaMarca LomaNegra LomaNegra = True
mismaMarca _ _ = False

(Ladrillo o _ q) == (Ladrillo o1 _ q1) = mismoTipo o o1 && q==q1
(Vigueta l m _) == (Vigueta l1 m1 _) = l==l1 && mismoMaterial m m1
(Cemento m _)==(Cemento m1 _) = mismaMarca m m1
  _ == _ = False

{--Ejemplos de ejecucion:
 -- (Cemento Minetti 100) == (Cemento LomaNegra 100)
 -- False
 -- (Cemento Minetti 100) == (Cemento Minetti 100)
 -- True
 -- (Cemento Minetti 100) == (Vigueta 4.2 Hierro 100)
 -- False
 -- (Ladrillo Hormigon Techo 200) == (Vigueta 4.2 Hierro 100)
 -- False
 -- (Ladrillo Hormigon Techo 200) == (Ladrillo Hormigon Pared 200)
 -- True-}

--Ejercicio 3  Queremos hacer un programa, para que el dueño de un vivero de árboles nativos pueda saber cuales de sus árboles están en condiciones de ser vendidos
--a) Definir un tipo recursivo ArbolesNativos, que permite guardar las características de cada árbol en esta época. El tipo  ArbolesNativos, tendrá dos constructores:
{-
1) EvolucionDelArbol, que tiene los siguientes parámetros: 
-String, para el nombre del árbol
-Estado (tipo enumerado con el estado del actual del del arbol, Seco, EnFlor, Verde, ConFrutos)
-Int  ( el tamaño de alto, entre 1 y 10)
-Int  ( el tamaño de diametro, entre 1 y 10)
-Int ( apreciación general, entre  1 y 10)
-ArbolesNativos, recursión con el resto de los árboles. 
2) NoHayMasArboles, que es un constructor sin parámetros, similar al de la lista vacía, para indicar que se terminaron los árboles.
-}

data Estado = Seco | EnFlor | Verde | ConFrutos deriving Show

type Nombre = String
type Alto = Int
type Diametro = Int 
type Tamaño = Int

data ArbolesNativos = NoHayMasArboles | EvolucionDelArbol Nombre Estado Alto Diametro Tamaño ArbolesNativos deriving Show

--funciones auxiliares
estadoConFrutos:: Estado -> Bool
estadoConFrutos ConFrutos = True
estadoConFrutos _ = False 
 
estadoEnFlor:: Estado ->Bool
estadoEnFlor EnFlor = True
estadoEnFlor _ = False
 
estadoVerde:: Estado ->Bool
estadoVerde Verde = True
estadoVerde _ = False

{-Las condiciones del arbol para  poder ser vendido se describen a continuación según las diferentes características:
-Si el árbol tiene Estado ConFrutos puede ser vendido. 
-Si el Estado es EnFlor  debe tener más de 7 en el diámetro o en el alto, y tener en la apreciación general al menos un 8.
-Si el árbol tiene Estado Verde, debe tener al menos un 9 en diámetro, 9 en altura y 9 en apreciación general. 

Programar la función paraVender, que toma como primer parámetro árboles del tipo ArbolesNativos, y como segundo parámetro el nombre del árbol de tipo String y
retorna un valor de tipo Bool, indicando si el árbol con ese nombre puede ser vendido
-}

paraVender:: ArbolesNativos -> String -> Bool
paraVender NoHayMasArboles _ = False
paraVender (EvolucionDelArbol n e a d ap resto) nombre |n==nombre = estadoConFrutos e ||(estadoEnFlor e && (d>7 || a>7) && ap>=8) || (estadoVerde e && d>=9 && a>=9 && ap>=9) || paraVender resto nombre
                                                       |otherwise = paraVender resto nombre 
 
 --Ejemplos de ejecucion
 --paraVender (EvolucionDelArbol "Naranjo" ConFrutos 5 5 5 (EvolucionDelArbol "Limon" Verde 9 9 9 (EvolucionDelArbol "Manzano" ConFrutos 6 6 6 NoHayMasArboles ))) "Naranjo"
 --True
 --paraVender (EvolucionDelArbol "Naranjo" ConFrutos 5 5 5 (EvolucionDelArbol "Limon" Verde 9 9 9 (EvolucionDelArbol "Manzano" ConFrutos 6 6 6 NoHayMasArboles ))) "Limon"
 --True

--c) función devolverAltura, toma una variable a de tipo ArbolesNativos, y como segundo argumento un nombre, que identifica al árbol, 
--y en caso que el árbol esté en a (con una altura h), retorna Just h y Nothing en caso contrario

devolverAltura :: ArbolesNativos -> String -> Maybe Int
devolverAltura NoHayMasArboles _ = Nothing
devolverAltura (EvolucionDelArbol n _ a _ _ resto) nombre |n==nombre = Just a
                                                          |otherwise = devolverAltura resto nombre
 
 --devolverAltura (EvolucionDelArbol "Naranjo" ConFrutos 5 5 5 (EvolucionDelArbol "Limon" Verde 9 9 9 (EvolucionDelArbol "Manzano" ConFrutos 6 6 6 NoHayMasArboles ))) "Limon"
 --Just 9
 --devolverAltura (EvolucionDelArbol "Naranjo" ConFrutos 5 5 5 (EvolucionDelArbol "Limon" Verde 9 9 9 (EvolucionDelArbol "Manzano" ConFrutos 6 6 6 NoHayMasArboles ))) "Palta"
 --Nothing
