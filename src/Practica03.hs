module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
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
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons b) = (Cons b)
fnn (Var p) = (Var p)
fnn (Not(Var p)) = (Not(Var p)) 
fnn (Not(Or p q)) = And (fnn(Not p)) (fnn(Not q))
fnn (Not(And p q)) = Or (fnn(Not p))(fnn(Not q))
fnn (Not(Impl p q)) = And (fnn p)(fnn(Not q))
fnn (Not(Not p)) =  fnn p 
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Impl p q) = Or (fnn (Not p)) (fnn q) 
fnn (Syss p q) =  And (Or (fnn(Not p)) (fnn q)) (Or (fnn(Not q)) (fnn p))

--Ejercicio 2
fnc :: Prop -> Prop
fnc p = distri ( fnn (p))

distri :: Prop -> Prop
distri (And p q) = And (distri p) (distri q)
distri (Or p q)  = distriOr (distri p) (distri q)
distri p         = p

distriOr :: Prop -> Prop -> Prop
distriOr (And p q) r = And (distri (Or p r)) (distri (Or q r))
distriOr r (And p q) = And (distri (Or p r)) (distri (Or q r))
distriOr p q         = Or p q

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

complemento :: Prop -> Prop
complemento (Not p) = p
complemento p       = Not p

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas p = [eliminarDuplicados (clausula p)]

clausula :: Prop -> Clausula
clausula (Or p q) = clausula p ++ clausula q
clausula p        = [p]

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
    case [ (l, complemento l) | l <- c1, complemento l `elem` c2 ] of
        ((l, lc):_) ->
            let c1' = filter (/= l) c1
                c2' = filter (/= lc) c2
            in eliminarDuplicados(c1' ++ c2')
        [] -> eliminarDuplicados(c1 ++ c2)

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = any (\l -> complemento l `elem` c2) c1

--Ejercicio 2
saturacion :: Prop -> Bool
saturacion p = saturar (clausulas (fnc p))

saturar :: [Clausula] -> Bool
saturar cls
    | [] `elem` cls = False   
    | otherwise =
        let nuevos = [ resolucion c1 c2 
                     | c1 <- cls, c2 <- cls,
                       hayResolvente c1 c2 ]
        in if all (`elem` cls) nuevos
           then if length cls > 20  -- limitamos para que no siga al infinito
                then let loop = loop in loop 
                else True                    
           else saturar (eliminarDuplicados (cls ++ nuevos))

eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = x : eliminarDuplicados (filter (/= x) xs)