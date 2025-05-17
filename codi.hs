-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Company (uCompany@campus.udg.edu)


-- CONTINGUT DE LA PRÀCTICA

-- definicions de tipus
data LT = Variable String | Aplicacio LT LT | Abstraccio String LT deriving Eq

instance Show LT where
    show (Variable a) = show a
    show (Aplicacio t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Abstraccio a t1) = "(\\" ++ show a ++ ". " ++ show t1 ++ ")"

data LTdB = VariabledB Int | AplicaciodB LTdB LTdB | AbstracciodB LTdB deriving Eq

instance Show LTdB where
    show (VariabledB a) = show a
    show (AplicaciodB t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (AbstracciodB t1) = "(\\" ++ ". " ++ show t1 ++ ")"

type Substitucio m v m' = [(m,v,m')]

type Context = String

-- freeAndboundVars, donat un LT retorna una tupla amb una llista de freeVars i una llista de boundVars
--freeAndboundVars :: LT -> ([],[])

-- subst, donat un LT i una Substitucio, retorna el mateix LT al que se li ha aplicat la Substitucio
--subst :: LT -> Substitucio -> LT

-- esta_normal, diu si LT ja està en forma normal
--esta_normal :: LT -> Bool
--esta_normal (Variable a) = True
--esta_normal t | t == (Variable _) = True
--              | t == (Aplicacio t1 t2) = (esta_normal t1) (&&) (esta_normal t2)
--              | t == (Abstraccio a t1) = (esta_normal t1)

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


exemple :: LT
exemple = Abstraccio "x" (Aplicacio (Variable "x") (Variable "y"))

exempledB :: LTdB
exempledB = AbstracciodB (AplicaciodB (VariabledB 0) (AbstracciodB (VariabledB 1)))

main :: IO ()
main = do
    putStrLn(show exemple)
    putStrLn(show exempledB)
