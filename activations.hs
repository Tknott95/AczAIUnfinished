
module Activation (
  actiSoft,
  actiRELU,
  backRELU,
  calcLoss',
  fwdLoss,
  fwdLossOneHot,
  getPredictions,
  getAcc
) where

import Control.Parallel.Strategies

-- add parr and rpar, from ghc docs about speed, l8r

esNum = 2.718281828459045

sumList xs = [sum x | x <- xs] `using` parList rseq
getExp xs = [clip0$ esNum**x | x <- xs] `using` parList rseq

-- normSoftmax xs mySum = [x/mySum | x <- xs]
normSoftmax xs mySum = (/mySum) <$> xs `using` parList rseq

maxSingle xs = [max 0 x | x <- xs] `using` parList rseq

-- this is awful @TODO fix ONLY WORKS WITH MULTIDIM ARRAY
maxX [x] = x
maxX (x:xs)
  | maxX xs > x = maxX xs
  | otherwise = x

-- 51
actiSoft :: [Double] -> [Double]
actiSoft xs = normSoftmax ijk  (sum ijk) `using` parList rseq where ijk = getExp(maxSingle xs)

actiRELU :: [Double] -> [Double]
actiRELU xs = max 0 <$> xs `using` parList rseq

backRELU :: [[Double]] -> [[Double]]
backRELU xs = fmap(\ijk -> [if x <= 0 then 0 else x | x <- ijk]) xs `using` parList rseq
{- -log(0) is infinity so i need to clip to 1^-? to 1^? -}
{- loop both lists by zipping -}
-- calcLoss :: [Double] -> [Double] -> Double
-- log x * target
-- calcLoss _xs _targets = sum ijk where ijk = [-log x * (_targets !! 0) | x <- _xs]

-- if GT 1 then clip
-- if LT | ET 0 then clip
clip0 :: Double -> Double
clip0 0 = 0.00001
clip0 _a = if _a < 0.00001 then 0.00001 else _a

-- clip7 :: Double -> Double
-- clip7 _a = if _a < 0.0000001 then 0.0000001 else _a

{- if GT 1^e-7 or LT 1^e7 then clip at that val, floats may clip precision already for now 
 - @TODO @NOTE -}

calcLoss' :: [Double] -> [Int] -> Double
calcLoss' _xs _targets = sum ijk 
 where 
   ijk = [-log(clip0 $ uncurry (*) x) `using` rseq | x <- ij] `using` parList rseq
   ij = zip _xs newTargets
   newTargets = [(fromIntegral i) | i <- _targets]
{- let z = calcLoss' [0.33, 1, 1] [1, 1, 1] -}
-- log x * target

{- was called backloss prios -}
{- @TODO @NOTE this will ahpopen on forward as well -}
-- @TODO I HAVE TO CLIP0 SO I DO NOT GET INFINITY
-- THIS IS A BACK PROPAGATION LOSS AND NOT A FORWARD, AS TO WHAT IS CAUSING AN INDEX ERROR WITH 2 AT THE LAST ITER
fwdLoss :: [Int] -> [[Double]] -> [Double]
fwdLoss _xs _ys = [-log(clip0 $ snd i !! fst i) `using` rseq  | i <- zip _xs _ys] 

-- @TODO build in oneHOTS - everything needs to be dynamic

-- move to lin.hs @TODO
-- mDimMats _xs _ys = zipWith(zipWith (*)) _xs _ys
mDimMats :: [[Double]] -> [[Double]] -> [[Double]]
mDimMats  i j= zipWith(zipWith (*)) i j  `using` parList rseq

negLogM :: [Double] -> [Double]
negLogM _xs = [-log(clip0 j) | j <-_xs  ] `using` parList rseq
-- negLogM = fmap(\j -> -log (clip0 j)) -- for nested parr

{- @TODO @NOTE this will ahpopen on forward as well -}
 -- @TODO make func that does this below
 -- sum ( _targets * ys )
-- oneHotTarget == True
-- `using` parList rseq
fwdLossOneHot :: [[Double]] -> [[Double]] -> [Double]
fwdLossOneHot _targets _ys = negLogM (sum <$> ijk)  `using` parList rseq
  where
    ijk = mDimMats _targets _ys

multMatDiv :: Double -> [[Double]] -> [[Double]]
multMatDiv _i _xs = fmap (fmap(\x -> x / _i)) _xs


crossEnpDivBoth :: [Double] -> [Double] -> [Double]
crossEnpDivBoth _xs _ys = [(-(fst i) / (snd i)) | i <- zip _xs _ys]
-- crossEntropyBackLoss
-- testTargets - dVals
-- actually loop through everything with this instead of the first
-- will do this a better way
-- @TODO INSTEAD OF DIVIND BY THE fromIntegral part, I CAN CREATE A FUCNTIONS THAT DIVIDES EACH BY IT VIA PARAMS AND JUST ABTRACT IT FOR NOW @TODO

crossEntropyBackLoss :: [[Double]] -> [[Double]] -> [[Double]]  --      THIS IS WHERE I CAN PULL THIS OUT INTO A FUNCTION TO HANDLE EACH OR USE A LAMBDA
crossEntropyBackLoss _xs _ys = multMatDiv (fromIntegral $ length (_ys))  [(crossEnpDivBoth (fst i) (snd i))  `using` rseq  | i <- zip _xs _ys]
-- the return will be the new dInputs

-- (\x -> \y -> y / 


-- crossEntropyBackLoss :: [[Double]] -> [[Double]] -> [Double]
-- crossEntropyBackLoss _xs _ys = [ (-(fst i !! 0) / (snd i !! 0) ) /  fromIntegral (length (snd i))   `using` rseq  | i <- zip _xs _ys] 
-- -- the return will be the new dInputs

-- grabbing accuracy, helper functions for such
-- seeing if this fixes the indx error - is it from lazy laoding?, NO
  -- @TODO add in nested conc
-- IT CAN't BE THIS FUCNITON CAUSING THE ISSUE, POSSIBLY THE UNCURRY? 
maxIndex :: Ord a => [a] -> Int
maxIndex xs = head ( filter ((== maximum xs) . (xs !!)) [0..])

getPredictions :: [[Double]] -> [Int]
getPredictions _xs = maxIndex <$> _xs `using` parList rseq

-- ifEqPair :: [(Int, Int)] -> [Int]
-- ifEqPair a = if (fst a) == (snd a) then 1 else 0
--intEqM _xs _ys = ifx  <$> zip _xs _ys
-- [ ifx ij | ij <- zip _xs _ys] 

intEqM :: [Int] -> [Int] -> [Int]
intEqM _xs _ys = ifx <$> zip _xs _ys `using` parList rseq
  where ifx a = if uncurry(==) a then 1 else 0

getMean :: [Int] -> Double
getMean _a =  ab / bb
  where
    ab = fromIntegral(sum _a)
    bb = fromIntegral(length _a)

getAcc :: [Int] -> [Int] -> Double
getAcc _as _bs = getMean (intEqM _as _bs)
