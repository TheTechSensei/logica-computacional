data Persona = Persona {nombre :: String,
                        apellido :: String,
                        estatura :: Float,
                        peso :: Float,
                        edad :: Int} deriving Show


-- Hacer que Persona se pueda ordenar por altura
instance Ord Persona where
    compare p1 p2 = compare (estatura p1) (estatura p2)

-- Hacer que Persona se pueda comparar, 2 personas son iguales si se llaman igual
instance Eq Persona where
    p1 == p2 = nombre p1 == nombre p2 --"p1 y p2 son iguales si y solo si se llaman igual"


--if y where
maximo :: Int -> Int -> Int
maximo a b = if a > b then a else b



mayorDeEdad :: Persona -> String
mayorDeEdad persona = if anios >= 18 then "Es mayor de edad" else "No es mayor de edad"
                        where anios = edad persona



maximoTres :: Int -> Int -> Int -> Int
maximoTres x y z = maximo x (maximo y z)
    where maximo a b = if a > b then a else b


--Casos
dummy :: Float -> Float
dummy x
    | x < 5 = x + 2
    | x <= 15 = sqrt x
    | otherwise = x

esBisiesto :: Int -> Bool
esBisiesto anio  
    | mod anio 400 == 0 = True 
    | mod anio 100 == 0 = False 
    | mod anio 4 == 0 = True 
    | otherwise = False


juan = (Persona {nombre = "Juan",apellido = "Sanchez",estatura = 1.72, peso = 68.5, edad = 22})
juancho = (Persona {nombre = "Juan",apellido = "Cordova",estatura = 1.63, peso = 62, edad = 25})
haskell = (Persona {nombre = "Haskell", apellido = "Perez",estatura = 1.50, peso = 52.3, edad = 18})

sonIguales :: Eq a => a -> a -> Bool
sonIguales p1 p2 = p1 == p2

maximo2 :: Ord a => a -> a -> a
maximo2 a b = if a > b then a else b

--EXTRAS:
--Una funcion que sume los elementos impares en una lista
sumaImpares :: [Int] -> Int
sumaImpares [] = 0
sumaImpares (x:xs) = if mod x 2 == 1 then x + sumaImpares xs else sumaImpares xs

--Funcion que suma fracciones
sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a, b) (c, d) = if (b==d) then (a+c, b) else (a*d + b*c , b*d)

--MCD
mcd :: Int -> Int -> Int
mcd a b = if b == 0 then a else mcd b (mod a b)


--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]