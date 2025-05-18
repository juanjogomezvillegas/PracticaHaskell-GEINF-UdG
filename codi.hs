import Distribution.PackageDescription (Condition(Var))
-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Company (uCompany@campus.udg.edu)

-- CONTINGUT DE LA PRÀCTICA

-- definicions de tipus
data LT = Va String | Ap LT LT | Ab String LT

instance Show LT where
    show (Va a) = a
    show (Ap t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Ab a t1) = "(\\" ++ a ++ ". " ++ show t1 ++ ")"

instance Eq LT where
    (==) (Va _) (Va _) = True
    (==) (Ap t1 t2) (Ap t1' t2') = (||) ((&&) (t1 == t1') (t2 == t2')) ((&&) (t1 == t2') (t2 == t1'))
    (==) (Ab _ t1) (Ab _ t1') = t1 == t1'

data LTdB = VadB Int | ApdB LTdB LTdB | AbdB LTdB deriving Eq

instance Show LTdB where
    show (VadB a) = show a
    show (ApdB t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (AbdB t1) = "(\\" ++ ". " ++ show t1 ++ ")"

type Substitucio m v m' = (m,v,m')

type Context = String

-- Funcions auxiliars

-- eliminarDuplicats, funció que elimina els elements duplicats d'una llista
eliminarDuplicats :: [String] -> [String]
eliminarDuplicats = foldr cond []
    where cond x l | x `elem` l = l
                   | otherwise = x:l

-- concatTuples, operador que concatena o intercalar dues llistes que són a dins d'una tupla
concatTuples :: ([String],[String]) -> ([String],[String]) -> ([String],[String])
(a,b) `concatTuples` (c,d) = (eliminarDuplicats (a ++ c),eliminarDuplicats (b ++ d)) 

-- Funcions principals

-- freeAndboundVars, donat un LT retorna una tupla amb una llista de freeVars i una llista de boundVars
freeAndboundVars :: LT -> ([String],[String])
freeAndboundVars t = freeAndboundVarsAux t [] []

freeAndboundVarsAux :: LT -> [String] -> [String] -> ([String],[String])
freeAndboundVarsAux (Va a) freeVars boundVars = if a `elem` boundVars then (freeVars,boundVars) else (a:freeVars,boundVars)
freeAndboundVarsAux (Ab a t1) freeVars boundVars = (freeAndboundVarsAux t1 freeVars (a:boundVars))
freeAndboundVarsAux (Ap t1 t2) freeVars boundVars = (freeAndboundVarsAux t1 freeVars boundVars) `concatTuples` (freeAndboundVarsAux t2 freeVars boundVars)

-- subst, donat un LT i una Substitucio, retorna el mateix LT al que se li ha aplicat la Substitucio
--subst :: LT -> Substitucio -> LT

-- esta_normal, diu si LT ja està en forma normal
esta_normal :: LT -> Bool
esta_normal (Va a) = True
esta_normal (Ap (Ab _ _) _) = False
esta_normal (Ab _ t1) = (esta_normal t1)
esta_normal (Ap t1 t2) = (&&) (esta_normal t1) (esta_normal t2)

-- beta_redueix, rep un LT que sigui un redex, i el resol
--beta_redueix :: LT -> LT
--beta_redueix (Ap (Ab a t) t') = subst t (Substitucio t a t')

-- redueix_un_n, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre normal
--redueix_un_n :: LT -> LT

-- redueix_un_a, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre aplicatiu
--redueix_un_a :: LT -> LT

-- l_normalitza_n, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre normal
--l_normalitza_n :: LT -> [LT]

-- l_normalitza_a, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre aplicatiu
--l_normalitza_a :: LT -> [LT]

-- normalitza_n, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre normal
--normalitza_n :: LT -> (Int,LT)

-- normalitza_a, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre aplicatiu
--normalitza_a :: LT -> (Int,LT)


-- a_deBruijn :: LT -> Context -> LTdB

-- de_deBruijn :: LTdB -> LT

-- Alguns combinadors i definicions del meta-llenguatge
iden :: LT
iden = (Ab "x" (Va "X"))

true :: LT
true = (Ab "x" (Ab "y" (Va "x")))

false :: LT
false = (Ab "x" (Ab "y" (Va "y")))

not :: LT
not = (Ab "t" (Ap false true))

cond :: LT -> LT -> LT -> LT
cond e e1 e2 = (Ap (Ap e e1) e2)

and :: LT
and = (Ab "x" (Ab "y" (cond (Va "x") (Va "y") false)))

tupla :: LT
tupla = (Ab "x" (Ab "y" (Ab "p" (Ap (Ap (Va "p") (Va "x")) (Va "y")))))

first :: LT
first = (Ab "x" (Ap (Va "x") true))

second :: LT
second = (Ab "x" (Ap (Va "x") false))

succ :: LT
succ = (Ab "n" (Ab "f" (Ab "x" (Ap ((Ap (Va "n") (Va "f"))) ((Ap (Va "f") (Va "x")))))))

prefn :: LT
prefn = (Ab "f" (Ab "p" (Ab "p" (Ap (Ap (Va "p") false) (cond (Ap first (Va "p")) (Ap second (Va "p")) (Ap (Va "f") (Ap second (Va "p"))))))))

prec :: LT
prec = (Ab "n" (Ab "f" (Ab "x" (Ap second (Ap (Ap (Va "n") (Ap (prefn) (Va "f"))) (Ab "p" (Ap (Ap (Va "p") true) (Va "x"))))))))

suma :: LT
suma = (Ab "m" (Ab "n" (Ab "f" (Ab "x" (Ap ((Ap (Va "m") (Va "f"))) ((Ap (Ap (Va "n") (Va "f")) (Va "x"))))))))

producte :: LT
producte = (Ab "m" (Ab "n" (Ab "f" (Ab "x" ((Ap (Ap (Va "m") (Ap (Va "n") (Va "f"))) (Va "x")))))))

eszero :: LT
eszero = (Ab "n" (Ap (Ap (Va "n") (Ab "x" false)) true))

y :: LT
y = (Ab "f" (Ap (Ab "x" (Ap (Va "f") (Ap (Va "x") (Va "x")))) (Ab "x" (Ap (Va "f") (Ap (Va "x") (Va "x"))))))

t :: LT
t = (Ap (Ab "x" (Ab "y" (Ap (Va "y") (Ap (Ap (Va "x") (Va "x")) (Va "y"))))) (Ab "x" (Ab "y" (Ap (Va "y") (Ap (Ap (Va "x") (Va "x")) (Va "y"))))))

main :: IO ()
main = do
    putStrLn(show t)
