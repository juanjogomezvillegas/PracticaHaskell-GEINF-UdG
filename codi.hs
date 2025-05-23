-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Guillem Pozo Sebastián (u1972840@campus.udg.edu)

-- CONTINGUT DE LA PRÀCTICA

-- Definicions de tipus

-- definició literal de la gramàtica del lambda càlcul
data LT = Va String | Ap LT LT | Ab String LT

-- fem instància de la classe Show i Eq al tipus de dades LT
-- definim la forma de mostrar un lambda terme
instance Show LT where
    show (Va a) = a
    show (Ap t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Ab a t1) = "(\\" ++ a ++ ". " ++ show t1 ++ ")"

-- definim la idea d'alpha-equivalència de dos lambda termes
instance Eq LT where
    (Va a) == (Va b) = a == b
    (Ap t1 t2) == (Ap t1' t2') = t1 == t1' && t2 == t2'
    (Ab _ t1) == (Ab _ t2) = t1 == t2
    _ == _ = False

-- definició de la gramàtica del lambda càlcul amb notació de bruijn
data LTdB = VadB Int | ApdB LTdB LTdB | AbdB LTdB

-- que també serà instància de la classe Show i Eq
instance Show LTdB where
    show (VadB a) = show a
    show (ApdB t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (AbdB t1) = "(\\" ++ ". " ++ show t1 ++ ")"

instance Eq LTdB where
    (==) t1 t2 = t1 == t2

-- definim una Substitució basant-nos en com es representa segons la teoria: M[v -> M'], és a dir, una operació de substitució d'una variable v per un lambda terme M' sobre un terme M
-- el terme M no el definim aquí, només definim [v -> M'] com [String -> LT]
data Substitucio = Sub String LT

-- definim un context per la notació de bruijn
type Context = String

-- Funcions auxiliars

-- getVar, donat una variable d'un lambda terme retorna la variable com un string
getVar :: LT -> String
getVar (Va a) = a

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
subst :: LT -> Substitucio -> LT
subst t s = substAux t s (freeAndboundVars t)

-- substAux, el mateix subst però rebent també la tupla amb les llistes de variables lliures i lligades
substAux :: LT -> Substitucio -> ([String],[String]) -> LT
substAux (Va a) (Sub v m') l = if a == v && a `elem` (snd l) then m' else (Va a)
substAux (Ab a t1) (Sub v m') l = if a == v then (Ab (getVar m') (substAux t1 (Sub v m') l)) else (Ab a (substAux t1 (Sub v m') l))
substAux (Ap t1 t2) (Sub v m') l = (Ap (substAux t1 (Sub v m') l) (substAux t2 (Sub v m') l))

-- esta_normal, diu si LT ja està en forma normal
esta_normal :: LT -> Bool
esta_normal (Va a) = True
esta_normal (Ap (Ab _ _) _) = False
esta_normal (Ab _ t1) = (esta_normal t1)
esta_normal (Ap t1 t2) = (&&) (esta_normal t1) (esta_normal t2)

-- beta_redueix, rep un LT que sigui un redex, i el resol
--beta_redueix :: LT -> LT
beta_redueix :: LT -> LT
beta_redueix (Ap (Ab a t1) t2) = subst t1 (Sub a t2)
beta_redueix t = t

-- redueix_un_n, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre normal
--redueix_un_n :: LT -> LT

-- redueix_un_a, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre aplicatiu
--redueix_un_a :: LT -> LT
redueix_un_a :: LT -> LT
redueix_un_a (Ap (Ab a t1) t2) = beta_redueix (Ap (Ab a t1) t2)
redueix_un_a (Ap t1 t2) =
    let t1' = redueix_un_a t1 in
        if t1 /= t1' then Ap t1' t2
        else
            let t2' = redueix_un_a t2 in
                if t2 /= t2' then Ap t1 t2'
                else Ap t1 t2
redueix_un_a (Ab a t) = Ab a (redueix_un_a t)
redueix_un_a t = t


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
iden = (Ab "x" (Va "x"))

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
