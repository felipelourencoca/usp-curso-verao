module Rec where 

data Nat = Z | Suc Nat deriving Show

natToInt :: Nat -> Int 
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n  

somar :: Nat -> Nat -> Nat 
somar x Z = x 
somar x (Suc n) = Suc (somar x n)

mult :: Nat -> Nat -> Nat 
mult x Z = Z 
mult x (Suc Z) = x 
mult x (Suc n) = somar x (mult x n)

fat :: Int -> Int 
fat n 
    | n <= 0 = 1
    | otherwise = n * fat (n - 1)

fatt :: Nat -> Nat 
fatt Z = Suc Z 
fatt (Suc n) = mult (Suc n) (fatt n)

cinco :: Nat 
cinco = Suc(Suc(Suc(Suc(Suc Z))))

--

data Func = Kons Double 
          | X 
          | Pol Func Int
          | Sen Func
          | Cos Func  
          | Exp Func 
          | Add Func Func 
          | Mult Func Func deriving Show 

ddx :: Func -> Func 
ddx (Kons k) = Kons 0
ddx X = Kons 1 
ddx (Pol f n) = 
    Mult (Mult (Kons (fromIntegral n)) (Pol f (n-1))) (ddx f)
ddx (Add f g) = Add (ddx f) (ddx g) 
ddx (Sen f) = Mult (Cos f) (ddx f)


-- CAP 5

data Bolsa a b = Bolsa {
    primeiro :: a, 
    segundo :: b 
} deriving Show



