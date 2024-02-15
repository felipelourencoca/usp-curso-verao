module Monadas where

import Control.Monad
import Distribution.Simple.GHC (getLibDir)


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
    (>>=) :: Talvez a -> (a -> Talvez b) -> Talvez b
    (Apenas x) >>= funcao = funcao x
    _ >>= _ = Nada



numeroPar :: Int-> Bool
numeroPar x 
    | even x    = True  
    | otherwise = False

{-
main :: IO ()
main = 
    putStrLn "Digite um valor: " >>= (\ naotemValor ->
    (readLn :: IO Int) ) >>= (\ v1 ->
    putStrLn "Digite outro valor: ") >>= (\ naotemValor2 ->
    (readLn :: IO Int) ) >>= (\ v2 ->
    putStrLn $ show (v1 + v2))
-}


main' :: IO ()
main' = 
    putStrLn "Digite um valor: " >>
    (readLn :: IO Int) >>= \ valor ->
    print $  boolTexto $numeroPar valor
    where
        boolTexto True = "Eh par"
        boolTexto False = "Eh impar" 


main :: IO ()
main = do
    putStrLn "Digite um valor: "
    v1 <- (readLn :: IO Int) 
    putStrLn "Digite outro valor: "
    v2 <- (readLn :: IO Int) 
    let vetor = [v1,v2]
        vetor2 = fmap (+1) vetor
    putStrLn $ show $ fmap (+1) vetor2



-- putStrLn :: String -> IO ()
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b 
-- m a -> (a -> m b) - m b

