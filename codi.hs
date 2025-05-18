import Distribution.PackageDescription (Condition(Var))
-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Company (uCompany@campus.udg.edu)


-- CONTINGUT DE LA PRÀCTICA

-- definicions de tipus
data LT = Variable String | Aplicacio LT LT | Abstraccio String LT

instance Show LT where
    show (Variable a) = show a
    show (Aplicacio t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Abstraccio a t1) = "(\\" ++ show a ++ ". " ++ show t1 ++ ")"

instance Eq LT where
    (==) (Variable _) (Variable _) = True
    (==) (Aplicacio t1 t2) (Aplicacio t1' t2') = (||) ((&&) (t1 == t1') (t2 == t2')) ((&&) (t1 == t2') (t2 == t1'))
    (==) (Abstraccio _ t1) (Abstraccio _ t1') = t1 == t1'

data LTdB = VariabledB Int | AplicaciodB LTdB LTdB | AbstracciodB LTdB deriving Eq

instance Show LTdB where
    show (VariabledB a) = show a
    show (AplicaciodB t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (AbstracciodB t1) = "(\\" ++ ". " ++ show t1 ++ ")"

type Substitucio m v m' = [(m,v,m')]

type Context = String

-- funcions auxiliars
-- concatTuples, operador que concatena o intercalar dues llistes que són a dins d'una tupla
concatTuples :: ([String],[String]) -> ([String],[String]) -> ([String],[String])
(a,b) `concatTuples` (c,d) = (a ++ c,b ++ d)

-- freeAndboundVars, donat un LT retorna una tupla amb una llista de freeVars i una llista de boundVars
freeAndboundVars :: LT -> ([String],[String])
freeAndboundVars t = freeAndboundVarsAux t [] []

freeAndboundVarsAux :: LT -> [String] -> [String] -> ([String],[String])
freeAndboundVarsAux (Abstraccio a t1) freeVars boundVars = (freeAndboundVarsAux t1 freeVars (a:boundVars))
freeAndboundVarsAux (Aplicacio t1 t2) freeVars boundVars = (freeAndboundVarsAux t1 freeVars boundVars) `concatTuples` (freeAndboundVarsAux t2 freeVars boundVars)
freeAndboundVarsAux (Variable a) freeVars boundVars = if a `elem` boundVars then (freeVars,boundVars) else (a:freeVars,boundVars)

-- subst, donat un LT i una Substitucio, retorna el mateix LT al que se li ha aplicat la Substitucio
--subst :: LT -> Substitucio -> LT

-- esta_normal, diu si LT ja està en forma normal
esta_normal :: LT -> Bool
esta_normal (Variable a) = True
esta_normal (Aplicacio (Abstraccio _ _) _) = False
esta_normal (Abstraccio _ t1) = (esta_normal t1)
esta_normal (Aplicacio t1 t2) = (&&) (esta_normal t1) (esta_normal t2)

-- beta_redueix, rep un LT que sigui un redex, i el resol
--beta_redueix :: LT -> LT

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
iden = (Abstraccio "x" (Variable "X"))

true :: LT
true = (Abstraccio "x" (Abstraccio "y" (Variable "x")))

false :: LT
false = (Abstraccio "x" (Abstraccio "y" (Variable "y")))

not :: LT
not = (Abstraccio "t" (Aplicacio false true))

cond :: LT -> LT -> LT -> LT
cond e e1 e2 = (Aplicacio (Aplicacio e e1) e2)

and :: LT
and = (Abstraccio "x" (Abstraccio "y" (cond (Variable "x") (Variable "y") false)))

tupla :: LT
tupla = (Abstraccio "x" (Abstraccio "y" (Abstraccio "p" (Aplicacio (Aplicacio (Variable "p") (Variable "x")) (Variable "y")))))

first :: LT
first = (Abstraccio "x" (Aplicacio (Variable "x") true))

second :: LT
second = (Abstraccio "x" (Aplicacio (Variable "x") false))

succ :: LT
succ = (Abstraccio "n" (Abstraccio "f" (Abstraccio "x" (Aplicacio ((Aplicacio (Variable "n") (Variable "f"))) ((Aplicacio (Variable "f") (Variable "x")))))))

suma :: LT
suma = (Abstraccio "m" (Abstraccio "n" (Abstraccio "f" (Abstraccio "x" (Aplicacio ((Aplicacio (Variable "m") (Variable "f"))) ((Aplicacio (Aplicacio (Variable "n") (Variable "f")) (Variable "x"))))))))

producte :: LT
producte = (Abstraccio "m" (Abstraccio "n" (Abstraccio "f" (Abstraccio "x" ((Aplicacio (Aplicacio (Variable "m") (Aplicacio (Variable "n") (Variable "f"))) (Variable "x")))))))

eszero :: LT
eszero = (Abstraccio "n" (Aplicacio (Aplicacio (Variable "n") (Abstraccio "x" false)) true))

y :: LT
y = (Abstraccio "f" (Aplicacio (Abstraccio "x" (Aplicacio (Variable "f") (Aplicacio (Variable "x") (Variable "x")))) (Abstraccio "x" (Aplicacio (Variable "f") (Aplicacio (Variable "x") (Variable "x"))))))

t :: LT
t = (Aplicacio (Abstraccio "x" (Abstraccio "y" (Aplicacio (Variable "y") (Aplicacio (Aplicacio (Variable "x") (Variable "x")) (Variable "y"))))) (Abstraccio "x" (Abstraccio "y" (Aplicacio (Variable "y") (Aplicacio (Aplicacio (Variable "x") (Variable "x")) (Variable "y"))))))

main :: IO ()
main = do
    putStrLn(show t)
