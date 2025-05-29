import Control.Monad.Cont (Cont)
-- Pràctica de Haskell
-- Copyright (c) 2025 Juan José Gómez Villegas (u1987338@campus.udg.edu), Guillem Pozo Sebastián (u1972840@campus.udg.edu)

-- CONTINGUT DE LA PRÀCTICA

-- Definicions de tipus

-- definició literal de la gramàtica del lambda càlcul
data LT = Va String | Ap LT LT | Ab String LT deriving Read

-- fem instància de la classe Show i Eq al tipus de dades LT
-- definim la forma de mostrar un lambda terme
instance Show LT where
    show (Va a) = a
    show (Ap t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Ab a t1) = "(\\" ++ a ++ ". " ++ show t1 ++ ")"

-- definim la idea d'equivalència de dos lambda termes
instance Eq LT where
    (==) (Va a) (Va b) = a == b
    (==) (Ap t1 t2) (Ap t1' t2') = (&&) (t1 == t1') (t2 == t2')
    (==) (Ab _ t1) (Ab _ t2) = t1 == t2
    (==) _ _ = False

instance Ord LT where
    compare :: LT -> LT -> Ordering
    compare t1 t2 | get_depth t1 == get_depth t2 = EQ
                  | get_depth t1 <= get_depth t2 = LT
                  | otherwise = GT

-- definició de la gramàtica del lambda càlcul amb notació de bruijn
data LTdB = VadB Int | ApdB LTdB LTdB | AbdB LTdB

-- que també serà instància de la classe Show i Eq
instance Show LTdB where
    show (VadB a) = show a
    show (ApdB t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (AbdB t1) = "(\\" ++ ". " ++ show t1 ++ ")"

-- definim la mateixa idea d'equivalència que teníem en els lambda termes pels lambda termes amb notació de Bruijn
instance Eq LTdB where
    (==) (VadB a) (VadB b) = a == b
    (==) (ApdB t1 t2) (ApdB t1' t2') = (&&) (t1 == t1') (t2 == t2')
    (==) (AbdB t1) (AbdB t2) = t1 == t2
    (==) _ _ = False

-- definim una Substitució basant-nos en com es representa segons la teoria: M[v -> M'], és a dir, una operació de substitució d'una variable v per un lambda terme M' sobre un terme M
-- el terme M no el definim aquí, només definim [v -> M'] com [String -> LT]
data Substitucio = Sub String LT

-- definim un context per la notació de bruijn
type Context = [(String,Int)]

-- Funcions auxiliars

-- get_depth, funció que donat un LT, retorna la profunditat del seu arbre de parsing
get_depth :: LT -> Int
get_depth (Va a) = 0
get_depth (Ab _ t) = get_depth t + 1
get_depth (Ap t1 t2) = (get_depth t1 + get_depth t2) + 1

-- es_redex, funció que donat un LT, retorna True si es un redex, False altrament
es_redex :: LT -> Bool
es_redex (Ap (Ab _ _) _) = True
es_redex _ = False

-- conte_redex, funció que determina si un terme conté qualsevol redex (en qualsevol nivell, equivalent a fer esRedex sobre tot el terme)
conte_redex :: LT -> Bool
conte_redex (Va _) = False
conte_redex (Ab _ t) | es_redex t = True
                     | otherwise = conte_redex t
conte_redex (Ap t1 t2) | es_redex (Ap t1 t2) = True
                       | es_redex t1 = True
                       | es_redex t2 = True
                       | otherwise = conte_redex t1 || conte_redex t2

-- eliminar_duplicats, funció que elimina els elements duplicats d'una llista
eliminar_duplicats :: Eq a => [a] -> [a]
eliminar_duplicats = foldr (\x xs -> if x `elem` xs then xs else x:xs) []

-- concat_tuples, operador que concatena o intercalar dues llistes que són a dins d'una tupla
concat_tuples :: Eq a => ([a],[a]) -> ([a],[a]) -> ([a],[a])
concat_tuples t1 t2 = (eliminar_duplicats (fst t1 ++ fst t2),eliminar_duplicats (snd t1 ++ snd t2))

-- llargada, funció que retorna la llargada d'una llista
llargada :: [a] -> Int
llargada = foldr (\_ y -> 1+y) 0

-- freeAndboundVarsAux, funció que construeix una tupla amb dues llistes que continguin les variables lliures (first) i lligades (second)
freeAndboundVarsAux :: [String] -> [String] -> LT -> ([String],[String])
freeAndboundVarsAux freeVars boundVars (Va a) | a `elem` boundVars = (freeVars,boundVars)
                                              | otherwise = (a:freeVars,boundVars)
freeAndboundVarsAux freeVars boundVars (Ab a t1) = freeAndboundVarsAux freeVars (a:boundVars) t1
freeAndboundVarsAux freeVars boundVars (Ap t1 t2) = freeAndboundVarsAux freeVars boundVars t1 `concat_tuples` freeAndboundVarsAux freeVars boundVars t2

-- Funcions principals

-- freeAndboundVars, donat un LT retorna una tupla amb una llista de freeVars i una llista de boundVars
freeAndboundVars :: LT -> ([String],[String])
freeAndboundVars = freeAndboundVarsAux [] []

-- subst, donat un LT i una Substitucio, retorna el mateix LT al que se li ha aplicat la Substitucio
subst :: LT -> Substitucio -> LT
subst (Va a) (Sub v t') | a == v = t'
                        | otherwise = Va a
subst (Ap t1 t2) (Sub v t') = Ap (subst t1 (Sub v t')) (subst t2 (Sub v t'))
subst (Ab a t1) (Sub v t') | a == v = Ab a t1
                           | a /= v && a `notElem` fst (freeAndboundVars t') = Ab a (subst t1 (Sub v t'))
                           | a /= v && a `elem` fst (freeAndboundVars t') = subst (alfa_conv t1 t' a (a ++ "\'")) (Sub v t')
                           | otherwise = Ab a t1
    where alfa_conv t1 t' v' v'' | v'' `notElem` fst (freeAndboundVars t') && v'' `notElem` fst (freeAndboundVars t1) = Ab v'' (subst t1 (Sub v' (Va v'')))
                                 | otherwise = alfa_conv t1 t' v' (v'' ++ "\'")

-- esta_normal, diu si LT ja està en forma normal
esta_normal :: LT -> Bool
esta_normal = not . conte_redex

-- beta_redueix, rep un LT que sigui un redex, i el resol
beta_redueix :: LT -> LT
beta_redueix (Ap (Ab a t1) t2) = subst t1 (Sub a t2)
beta_redueix (Ab _ t) = beta_redueix t
beta_redueix t = t

-- redueix_un, funció d'ordre superior que evita la repetició de codi entra la forma normal i l'aplicatiu
redueix_un :: (LT -> LT) -> LT -> LT
redueix_un f (Ap m n) = f (Ap m n)
redueix_un f (Ab x t) = Ab x (redueix_un f t)

-- redueix_un_n, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre normal
redueix_un_n :: LT -> LT
redueix_un_n = redueix_un casbase
    where casbase (Ap m n) | es_redex (Ap m n) = beta_redueix (Ap m n)
                           | not (esta_normal m) = Ap (redueix_un casbase m) n
                           | not (esta_normal n) = Ap m (redueix_un casbase n)
                           | otherwise = Ap m n

-- redueix_un_a, rep un LT, i retorna el LT resultant d'aplicar la primera beta-reducció segons l'ordre aplicatiu
redueix_un_a :: LT -> LT
redueix_un_a = redueix_un casbase
    where casbase (Ap m n) | not (esta_normal m) = Ap (redueix_un casbase m) n
                           | not (esta_normal n) = Ap m (redueix_un casbase n)
                           | es_redex (Ap m n) = beta_redueix (Ap m n)
                           | otherwise = Ap m n

-- l_normalitza, funció d'ordre superior que evita la repetició de codi entra la forma normal i l'aplicatiu
l_normalitza :: (LT -> LT) -> LT -> [LT]
l_normalitza f t | esta_normal t = t:[]
                 | otherwise = t:l_normalitza f t'
    where t' = f t

-- l_normalitza_n, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre normal
l_normalitza_n :: LT -> [LT]
l_normalitza_n = l_normalitza (redueix_un_n)

-- l_normalitza_a, rep un LT, i retorna una llista de LT's que sigui una seqüència de beta-reduccions, segons l'ordre aplicatiu
l_normalitza_a :: LT -> [LT]
l_normalitza_a = l_normalitza (redueix_un_a)

-- normalitza, funció d'ordre superior que evita la repetició de codi entra la forma normal i l'aplicatiu
normalitza :: (LT -> [LT]) -> LT -> (Int, LT)
normalitza f t = (llargada (lpassos t) - 1,last (lpassos t))
    where lpassos = f

-- normalitza_n, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre normal
normalitza_n :: LT -> (Int, LT)
normalitza_n = normalitza (l_normalitza_n)

-- normalitza_a, rep un LT, i retorna una tupla amb el nombre de passos, més el LT en forma normal, seguint l'ordre aplicatiu
normalitza_a :: LT -> (Int, LT)
normalitza_a = normalitza (l_normalitza_a)

-- Extra: Notació de Bruijn

-- funcions auxiliars

-- get_cont_aux, funció que per cada variable d'una llista li assigna un numero correlatiu de 0 a n
get_cont_aux :: [String] -> Int -> Context
get_cont_aux (x:xs) n | n < (llargada (x:xs)) = (x,n):get_cont_aux xs (n+1)
                      | otherwise = [] ++ [(x,n)]

-- get_cont, funció que donat un LT, retorna el seu Context
get_cont :: LT -> Context
get_cont t = get_cont_aux ls 0 ++ get_cont_aux lf ((llargada ls) + 1)
    where lf = fst (freeAndboundVars t)
          ls = snd (freeAndboundVars t)

-- mapeja_str_to_int, funció que mapeja una cadena de text (variable) cap a un enter (posició o distància en lambdes)
mapeja :: Eq a => a -> [(a,b)] -> b
mapeja a (t:ts) = if a == fst t then snd t else mapeja a ts

-- funcions principals

-- a_deBruijn, funció que rep un LT i un Context, i el passa a LTdB
-- p.e. a_deBruijn (Ab "x" (Va "x")) (get_cont (Ab "x" (Va "x")))
a_deBruijn :: LT -> Context -> LTdB
a_deBruijn (Va a) c = VadB (mapeja (head (filter (==a) (map fst c))) c)
a_deBruijn (Ap t1 t2) c = ApdB (a_deBruijn t1 c) (a_deBruijn t2 c)
a_deBruijn (Ab _ t) c = AbdB (a_deBruijn t c)

-- de_deBruijn, funció que rep un LTdB i el passa a LT
de_deBruijn :: LTdB -> LT
de_deBruijn (VadB a) = (Va "a")

-- Alguns combinadors i definicions del meta-llenguatge

iden :: LT
iden = Ab "x" (Va "x")

true :: LT
true = Ab "x" (Ab "y" (Va "x"))

false :: LT
false = Ab "x" (Ab "y" (Va "y"))

notDef :: LT
notDef = Ab "t" (Ap false true)

cond :: LT
cond = Ab "e" (Ab "e1" (Ab "e2" (Ap (Ap (Va "e") (Va "e1")) (Va "e2"))))

andDef :: LT
andDef = Ab "x" (Ab "y" (Ap (Ap (Ap cond (Va "x")) (Va "y")) false))

orDef :: LT
orDef = Ab "x" (Ab "y" (Ap (Ap (Ap cond (Va "x")) true) (Va "y")))

xorDef :: LT
xorDef = Ab "x" (Ab "y" (Ap (Ap (Ap cond (Va "x")) (Ap notDef (Va "y"))) (Va "y")))

tupla :: LT
tupla = Ab "x" (Ab "y" (Ab "p" (Ap (Ap (Va "p") (Va "x")) (Va "y"))))

first :: LT
first = Ab "x" (Ap (Va "x") true)

second :: LT
second = Ab "x" (Ap (Va "x") false)

succDef :: LT
succDef = Ab "n" (Ab "f" (Ab "x" (Ap (Ap (Va "n") (Va "f")) (Ap (Va "f") (Va "x")))))

prefn :: LT
prefn = Ab "f" (Ab "p" (Ap (Ap tupla false) (Ap (Ap (Ap cond (Ap first (Va "p"))) (Ap second (Va "p"))) (Ap (Va "f") (Ap second (Va "p"))))))

prec :: LT
prec = Ab "n" (Ab "f" (Ab "x" (Ap second (Ap (Ap (Va "n") (Ap prefn (Va "f"))) (Ap (Ap tupla true) (Va "x"))))))

suma :: LT
suma = Ab "m" (Ab "n" (Ab "f" (Ab "x" (Ap (Ap (Va "m") (Va "f")) (Ap (Ap (Va "n") (Va "f")) (Va "x"))))))

producte :: LT
producte = Ab "m" (Ab "n" (Ab "f" (Ab "x" (Ap (Ap (Va "m") (Ap (Va "n") (Va "f"))) (Va "x")))))

eszero :: LT
eszero = Ab "n" (Ap (Ap (Va "n") (Ab "x" false)) true)

g :: LT
g = Ab "x" (Ap (Ab "y" (Ab "x" (Ap (Va "y") (Va "y")))) (Ab "y" (Ab "x" (Ap (Va "y") (Va "y")))))

gprima :: LT
gprima = Ab "x" (Ap (Va "x") (Ap (Ab "y" (Ab "x" (Ap (Va "x") (Ap (Va "y") (Va "y"))))) (Ab "y" (Ab "x" (Ap (Va "x") (Ap (Va "y") (Va "y")))))))

y :: LT
y = Ab "f" (Ap (Ab "x" (Ap (Va "f") (Ap (Va "x") (Va "x")))) (Ab "x" (Ap (Va "f") (Ap (Va "x") (Va "x")))))

t :: LT
t = Ap (Ab "x" (Ab "y" (Ap (Va "y") (Ap (Ap (Va "x") (Va "x")) (Va "y"))))) (Ab "x" (Ab "y" (Ap (Va "y") (Ap (Ap (Va "x") (Va "x")) (Va "y")))))

factorial :: LT -> LT
factorial cf = Ap cf (Ab "f" (Ab "n" (Ap (Ap (Ap cond (Ap eszero (Va "n"))) u) (Ap (Ap producte (Va "n")) (Ap (Va "f") (Ap prec (Va "n")))))))

-- naturals
zero :: LT
zero = Ab "f" (Ab "x" (Va "x"))

u :: LT
u = snd (normalitza_n (Ap succDef zero))

dos :: LT
dos = snd (normalitza_n (Ap succDef (Ap succDef zero)))

tres :: LT
tres = snd (normalitza_n (Ap succDef (Ap succDef (Ap succDef zero))))

quatre :: LT
quatre = snd (normalitza_n (Ap succDef tres))

cinc :: LT
cinc = snd (normalitza_n (Ap succDef quatre))

sis :: LT
sis = snd (normalitza_n (Ap succDef cinc))

set :: LT
set = snd (normalitza_n (Ap succDef sis))

vuit :: LT
vuit = snd (normalitza_n (Ap succDef set))

nou :: LT
nou = snd (normalitza_n (Ap succDef vuit))

deu :: LT
deu = snd (normalitza_n (Ap succDef nou))

-- Funció principal

main :: IO ()
main = do
    putStrLn "Entra el teu nom:"
    name <- getLine
    putStrLn ("Hola " ++ name ++ ", entra un lambda-terme:")
    lt <- getLine
    let t = read lt :: LT
    print t
