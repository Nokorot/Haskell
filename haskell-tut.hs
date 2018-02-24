import Data.List
import System.IO

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

eo :: Double -> [Double]
eo x = 1 : [b * x/a | (a,b) <- zip [1..] (eo x) ]
texp x = sum (take (round $ (sqrt x) * 20) (eo x))
tsin x = sum [ ((eo x) !! i) * fromIntegral (2 - (mod i 4)) | i <- [1,3..100]]


--Newtons metode
func x = (x - 3)*(x - 1)*(x - 1)

done (x:xs)
      | abs (x - (head xs)) > 0 = done xs
      | otherwise               = x 

newtons f x0 x1 = do
    let xs = x0 : x1 : [ ( b - (b - a) / (1 - (f a) / (f b)) ) | (a, b) <- zip xs (tail xs)]
    done xs
    
    
-- Eulers metode

dxdt x t = -2 * x
euler x' x0 dt = do
    let txs = (0, x0) : [(t+dt, x + (x' x (t+dt)) * dt) | (t, x) <- txs]
    txs
    
-- Eulers metode, andre orden

dxdt2 t x v = -(1 * 10 * (sin x))
euler2 x'' x0 v0 dt = do
    let txv t x v = (t+dt, x + v*dt, v)
    let txvs = (0, x0, v0) : [(txv t x (v+(x'' t x v))) | (t, x, v) <- txvs]
    txvs
    
eTest = (take 100 (euler2 dxdt2 1 0 0.001))
myXs = [x | (t,x,v) <- eTest]
myVs = [v | (t,x,v) <- eTest] 
    
-- Eulers midtpunktmetode    

eulerM x' x0 dt = do
    let dx x t = dt * ( x' (x + (x' x t)*dt/2) (t + dt/2) )
    let txs = (0, x0) : [ (t+dt, x + (dx x t)) | (t, x) <- txs ]
    txs


-- Binominal funcsjonen

binom n 0 = 1
binom n k
    | n < k         = 0
    | n-k < k       = binom n (n-k)
    | otherwise     = n / k * binom (n-1) (k-1)







