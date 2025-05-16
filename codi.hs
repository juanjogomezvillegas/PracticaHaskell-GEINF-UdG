
-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Company (uCompany@campus.udg.edu)


-- CONTINGUT DE LA PRÀCTICA

data LT = Char deriving (Show,Eq)

data LTdB = Char deriving (Show,Eq)

type Substitucio = String

type Context = String

-- freeAndboundVars, donat un LT retorna una tupla amb una llista de freeVars i una llista de boundVars
freeAndboundVars :: LT -> ([],[])

-- subst, donat un LT i una Substitucio, retorna el mateix LT al que se li ha aplicat la Substitucio
subst :: LT -> Substitucio -> LT

-- esta_normal, diu si LT ja està en forma normal
esta_normal :: LT -> Bool

-- beta_redueix, rep un LT que sigui un redex, i el resol
beta_redueix :: LT -> LT

-- redueix_un_n, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre normal
redueix_un_n :: LT -> LT

-- redueix_un_a, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre aplicatiu
redueix_un_a :: LT -> LT

-- l_normalitza_n, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre normal
l_normalitza_n :: LT -> [LT]

-- l_normalitza_a, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre aplicatiu
l_normalitza_a :: LT -> [LT]

-- normalitza_n, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre normal
normalitza_n :: LT -> (Int,LT)

-- normalitza_a, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre aplicatiu
normalitza_a :: LT -> (Int,LT)


a_deBruijn :: LT -> Context -> LTdB

de_deBruijn :: LTdB -> LT
