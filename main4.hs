

tamanho :: String -> Int 
tamanho = length 

ehPar :: Int -> Bool 
ehPar = even 

data Logado 

data NaoLogado 

newtype Cookie a = Cookie String 
    deriving Show 

login :: String -> Cookie Logado 
login x
    | x == "a@b.com" = Cookie x 
    | otherwise = error "N logado"

rota1 :: Cookie Logado -> String 
rota1 _ = "Rota 1"

loginFalso :: String -> Cookie NaoLogado 
loginFalso _ = Cookie "falso"

{-*

data Maybe a = Just a | Nothing

class Functor f where 
    fmap :: (a -> b) -> (f a -> f b)

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

*-}

safeDiv :: Double -> Double -> Maybe Double 
safeDiv x 0 = Nothing 
safeDiv x y = Just (x/y)

data Bolsa a = Bolsa a a Int deriving Show 

instance Functor Bolsa where 
    fmap f (Bolsa x y z) = Bolsa (f x) (f y) z


data Talvez a = Nada | Apenas a deriving Show


instance Functor Talvez where   
    fmap :: (a -> b) -> Talvez a -> Talvez b
    fmap funcao (Apenas x) = Apenas (funcao x)



instance Applicative Talvez where
    pure  = Apenas 
    (Apenas func) <*> (Apenas valor) = Apenas ( func valor)
    _ <*> _ = Nada


instance Monad Talvez where
    return = pure
    (Apenas x) >>= funcao = funcao x
    _ >>= _ = Nada


tiraContexto :: Maybe Int -> Int
tiraContexto (Just x) = x
tiraContexto Nothing = 0

