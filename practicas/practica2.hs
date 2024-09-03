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

-- Función para obtener las variables únicas de una fórmula
variables :: Prop -> [String]
variables = variables' []
  where
    variables' acc (Var p) = if p `elem` acc then acc else p : acc
    variables' acc (Cons _) = acc
    variables' acc (Not p) = variables' acc p
    variables' acc (And p q) = variables' (variables' acc p) q
    variables' acc (Or p q) = variables' (variables' acc p) q
    variables' acc (Impl p q) = variables' (variables' acc p) q
    variables' acc (Syss p q) = variables' (variables' acc p) q

--Función que devuelve la lista de todos los subconjuntos de x
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

-- Función que evalúa una fórmula bajo un estado dado
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var p) estado = p `elem` estado
interpretacion (Cons b) _ = b
interpretacion (Not p) estado = not (interpretacion p estado)
interpretacion (And p q) estado = interpretacion p estado && interpretacion q estado
interpretacion (Or p q) estado = interpretacion p estado || interpretacion q estado
interpretacion (Impl p q) estado = not (interpretacion p estado) || interpretacion q estado
interpretacion (Syss p q) estado = interpretacion p estado == interpretacion q estado

-- Función que devuelve todos los estados posibles para evaluar una fórmula como verdadera
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = filter (\estado -> interpretacion f estado) (conjPotencia (variables f))

-- Función que verifica si una fórmula es una tautología
tautologia :: Prop -> Bool
tautologia f = all (\estado -> interpretacion f estado) (conjPotencia (variables f))

-- Función que verifica si una fórmula es una contradicción
contradiccion :: Prop -> Bool
contradiccion f = all (\estado -> not (interpretacion f estado)) (conjPotencia (variables f))

-- Función que verifica si una interpretación es un modelo de una fórmula
esModelo :: Estado -> Prop -> Bool
esModelo estado f = interpretacion f estado

-- Función que devuelve la lista de todos los modelos de una fórmula
modelos :: Prop -> [Estado]
modelos f = filter (\estado -> interpretacion f estado) (conjPotencia (variables f))

-- Función que verifica si una fórmula es válida
esValida :: Prop -> Bool
esValida f = all (\estado -> interpretacion f estado) (conjPotencia (variables f))

-- Función que verifica si una fórmula es insatisfacible
esInsatisfacible :: Prop -> Bool
esInsatisfacible f = all (\estado -> not (interpretacion f estado)) (conjPotencia (variables f))

-- Función que verifica si una fórmula es satisfacible
esSatisfacible :: Prop -> Bool
esSatisfacible f = any (\estado -> interpretacion f estado) (conjPotencia (variables f))
