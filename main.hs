import Net
import Lin
import Layers
import Activation

import Colors as C

import System.Random

import StaticData as SD
import Data.Bool


--  @TODO OPTIMIZER NEEDS TO BE MOVED AGAIN

-- @TODO change to unboxed arrays - vectors - or use repa which handles such things under the hood 
-- I have also come to findout that repa has a built in image loader. 
-- possibly an MVP w/ linked lists whilst converting over to repa
-- then one day could build a new lb with vectors and nested conc and parr
-- @TODO - @NOTES - @ICEBOX
-- haskell arrays are 1D arrays internally
-- OR USE accelerate:FOR GPUs 

-- @NOTES
-- My code is dogshit as it still is not refactored nor am I chaining functions with a larger type wrapped, which is more computation - sadly
-- how can I wrap these functions so I can handle them in a better way? Activation changes on layer so maybe a way to add  [layer00 acti00 layer01 acti01] ...
---  to then call recursively. This could handle all of the nonsense under the hood and make my architecture meodular once argMaxing
-- this also allows for me to make dyna,ic activations so the AI, itself, can make these switches later on
--  my idea is using another thread/graph that is freudian and using markovs. A way to detect how to do something by following a philosophy similar to "flow state".

-- For the list of functions, I have a few ideas still not yet sound enough for notes

-- USE llvm-9.0 w/ stack for acc to work
-- only using gpu acc onl linux - fuck windows

-- USE uarrays rather than linked lists, do after an mvp
-- is loss calc'd after backpropr or before? I would imagine after
-- why have I never, consciously, "foci'd" into such an important idea
-- only RELU first layer - only softmax the second
-- keep cttrl over the fzz and fuse

data IOptimizer = IOptimizer { 
  currEpoch :: Int,
  learningRate :: Double,
  bestLoss :: Double,
  bestLayers :: [Layer],
  layerSize :: [(Int, Int)]
} deriving (Show)

newtype INet = INet {
  allLayers :: [Layer]
} deriving Show

--- how should I handle dLayers? an internal layer that is passed down to then be thrown away back into an orig layer?
-- the code is getting sloppy and I would love to make it plug and play where you can easily go from one method of graphing to another

epochs = 137 :: Int

-- For my optimizer 0.1.1, I will be passing randListCstm and randLayerCstm 
-- to pass my 
-- actuals, not derivs
-- weights = (learningRate*) <$> dWeights 
-- biases (learningRate*) <$> dBiases 

-- REFACTOR BY PACKAGING UP MY FUNCTIONS

-- FWD LOSS DOESN'T WORK ON ONE HOT ENCODED TARGETS
-- DRUNK DEBUGGIN BBY

-- MY INDEX ON MY TARGET [2] IS TOO LARGE ON A FUNCTION THAT ONLY HAS [0..1], FIND THAT BUG 

{- @NOTES
 - make a system where I can add functions to a list/tuple to then call for each layer where I do not have to set each time 
 - add in loss b4 back prop?
 - add in backprop
 - add in loss
 - use a maybe or just check a [[]]  in param - maybe is best way yet more expensive and pointless here - I think
 - -}

-- argMaxing an index of hacked onHot targets which always will be 0 bcz they only have a length of 1
-- this is an issue, I need to handle this better when targets are not onehot encoded
-- this is breaking accuracy on this data with the oneHotTargs hack being one val as state above

{- @TODO make an optimizer type to handle bestLoss acc, etc -}
-- moving to a do block for getArgs
-- epochs :: Int
-- epochs = 13337
-- epochs = 1034
-- epochs <- getArgs[1]

-- might rmv this code and just zip
theData = SD.testNetDataAlt
theTargets = SD.testNetTargetsAlt
dataZipped = zip theTargets theData

testTargets = theTargets
testTargetsOneHot = [[i] | i <- theTargets]

-- FIXED THIS BUG BELOW, I WAS INIT WITH AN OLD FUNC - will doble check everythi nglater just to eb safe with tests
-- @TODO issue with dot prod by inputs * weight giving   m of len(inputs)

oneHotEncodedTargets :: Bool
oneHotEncodedTargets = False

-- @TODO move to diff file
-- int2Double = fmap (\k -> toDouble k) where toDouble = fmap(\j -> fromIntegral j)
-- int2Double = fmap (\k -> toDouble <$> k) where toDouble = (fromIntegral)
int2Double :: [[Int]] -> [[Double]]
int2Double = fmap (toDouble <$>) where toDouble = fromIntegral


l1 = Layer {
  inputs  = [[1.0, -3.0, -4.0]],
  weights = [[5.0, 6.0, 7.0]],
  biases  = [[0.2]]
}

iOptimizer = IOptimizer {
  currEpoch = 0,
  learningRate = 1.0,
  bestLoss = 100.0,
  bestLayers = [l1],
  layerSize = [(0, 0)]
}

testInputs = theData
mockI2 = theData

-- testInputs = [[0.1, 0.11, 0.985], [0.2, 0.21, 0.985], [0.61, 0.21, 0.385]]

-- mockI = [4.8, 1.21, 2.385]
-- mockI2 = [[0.1, 0.11, 0.385], [0.2, 0.21, 0.685]]

-- testVals = [[0.7, 0.1, 0.2], [0.1, 0.5, 0.4], [0.02, 0.9, 0.08]]
-- testTargets = [0, 1, 1]

-- testTargetsOneHot = [[1, 0, 0],[0, 1, 0],[0, 1, 0]]

-- Wrap [Layers] in a type

loopNTimes :: Int -> Bool ->  Bool -> INet -> IOptimizer -> IO ()
loopNTimes 0 _ _ _  _ = return ()
loopNTimes n firstEpoch betterLoss myNetLayer _optimizer = do
  putStrLn (b_ylw++"loop("++show (n-1)++")"++clr)

  -- let tLayer = myLayer
  -- -- = Layer {
  -- --   inputs = inputs myLayer,
  -- --   weights = (weights myLayer),
  -- --   biases = (biases myLayer)
  -- -- }
  -- {- iDLayer mockI2 (rSeed+n) -}

  -- putStrLn ("\n\n"++C.d_ylw++" tLayer [[a]] "++C.clr)
  -- print $ tLayer
  -- @TODO fix randSeed, it is too fast for this method.
  -- @FIXED using my old alt old method
  mySeed <- Lin.randSeed
  putStrLn ("\n"++d_ylw++"randSeed: "++show mySeed++clr)

  -- @TODO make this more dynamic in a where
  let iNetLay00 =  head $ allLayers myNetLayer
  let iNetLay01 =  allLayers myNetLayer !! 1
  -- let iNetLay02 =  allLayers myNetLayer !! 2

  let lay0Rows = length $ inputs iNetLay00
  let lay0Cols =  length $ inputs iNetLay00 !! 0
  -- let lay00 = myLayer
  -- @TODO fix randSeed, it is too fast for this method.
  -- THIS IS FOR WEIGHTS ONLY, I NEED TO TWEAK INSTEAD AFTER FIRST EPOCH ONCE SEGMENTATION FAULT ISSUE IS SOLVED
  let lSize = if firstEpoch then [(lay0Rows, lay0Cols), (2, 6)] else layerSize _optimizer
  

  let lyrForWeightsAndBiases00 = if firstEpoch then (iDLayerFromInputs (inputs $ iNetLay00) mySeed ) else Layer { inputs = inputs $ iNetLay00, weights = iLayerUpdateWeights (lSize !! 0) mySeed (weights $ iNetLay00), biases = iLayerUpdateBiases (lSize !! 0) mySeed  (biases $ iNetLay00) }

  let lay00 = Layer {
    -- weights tmpLayer handles randomness
    inputs = inputs iNetLay00,
    weights = weights lyrForWeightsAndBiases00,
    biases = biases lyrForWeightsAndBiases00
  }

  -- print $ inputs lay00
  -- rand weight hack for now
  -- if betterLoss = True then weights = myLayer weights
  -- else weights = randomWeights

  -- let lSize = [(length(testNetData), (length(TestNetData !! 0))), (12, 3), (3, 12)]

 -- these functions need to be simple - this is fucking trash
 -- I was waiting for MVP before a refactor and cleanup .. 
 -- how long can I leave this trash ass code prior to MVP is the real question
  let lay00_dot = iSetInputs (transpose $ Lin.matDot (inputs lay00) (weights lay00) (head $ biases lay00)) lay00
  let lay00_actiRELU = iSetInputs (Activation.actiRELU <$> inputs lay00_dot) lay00_dot
  -- let lay00_actiSoft = iSetInputs (Activation.actiSoft <$> inputs lay00_actiRELU) lay00_actiRELU
  -- remove soft

  let lyrForWeightsAndBiases01 = if firstEpoch then iDenseWithInputs (inputs lay00_actiRELU) 6 2 (mySeed+32342) else Layer { inputs = inputs $ iNetLay01, weights = iLayerUpdateWeights (lSize !! 1) mySeed (weights $ iNetLay01), biases = iLayerUpdateBiases (lSize !! 1) mySeed  (biases $ iNetLay01) }
 -- from 100  12 to 100 100
  let lay01 = Layer {
    -- weights tmpLayer handles randomness
    inputs = inputs lay00_actiRELU,
    weights = weights lyrForWeightsAndBiases01,
    biases = biases lyrForWeightsAndBiases01
  }

  -- print $ inputs lay01
  -- lay01 = iDenseWithInputs (inputs lay00_actiSoft) 12 3 (mySeed+32342)
  
  -- let lay01_tmp = iDenseWithInputs (inputs lay00_dot) 12 3 (mySeed+32342)
  -- let lay01 = Layer {
  --   inputs = inputs lay01_tmp,
  --   weights = if firstEpoch then weights lay01_tmp else iLayerUpdateWeights (lSize !! 0) {- layer !! 0 form prev epoch, need to pass it along with 3 -}
  -- }
  let lay01_dot = iSetInputs (transpose $ Lin.matDot (inputs lay01) (weights lay01) (head $ biases lay01)) lay01
  -- remove relu
  -- let lay01_actiRELU = iSetInputs (Activation.actiRELU <$> inputs lay01_dot) lay01_dot
  let lay01_actiSoft = iSetInputs (Activation.actiSoft <$> inputs lay01_dot) lay01_dot



  let actiSoftSum = sum <$> inputs lay01_actiSoft
  --  putStrLn $ alt++" actiSoft sum"++clr
  --   ++ show actiSoftSum 


  let fwdLoss = Activation.fwdLoss testTargets (inputs lay01_actiSoft)
  -- was named wrong and as bckLoss

  -- @TO_TEST 
  -- let bckLossTest = Activation.fwdLoss testTargets (inputs lay02_actiSoft) 
  let fwdLossTestOneHot = Activation.fwdLossOneHot (int2Double testTargetsOneHot) (inputs lay01_actiSoft)

  let fwdLossMean = Lin.calcMean fwdLoss
  putStrLn $ alt3++" crossEntropy fwdLossTest"++clr
    ++ show fwdLossMean -- Mean - remove mean if NaN !! index error and you can see a NaN is in the fwdLoss

  -- this is fucking up bcz I am turning basic vals into one hot encoded when they aren't
  -- @TODO make an if to handle this diff for non-onehot instead of my hack of wrappeing each w/ [a]
  let myPredictionsInputs = Activation.getPredictions $ inputs lay01_actiSoft
  
  -- print $ inputs myLayer
  -- print $ inputs lay02_actiSoft
  putStrLn $ alt4++"\n crossEntropy myPredictionsInputs - lay02_actiSoft output"++clr
    ++ show myPredictionsInputs
  -- print $ Activation.getPredictions $ inputs lay02_actiSoft

  -- THIS IS TO TEST WITHOUT PULLING MY IF APART. WILL RMV AFTER MVP
  {- make one for no non-one-hot values then apply and if oneHotEncoded == False -}
  -- let myPredictionsTargets = Activation.getPredictions $ int2Double testTargetsOneHot
  -- putStrLn $ alt4++"\n crossEntropy myPredictionsOneHot - testTargets"++clr
  --   ++ show myPredictionsTargets

  let myPredictions = if oneHotEncodedTargets then Activation.getPredictions $ int2Double testTargetsOneHot else testTargets
  putStrLn $ alt4++"\n crossEntropy myPredictionsTargetsOneHot - testTargets"++clr 
    ++ show myPredictions


  let myAccuracy = Activation.getAcc myPredictionsInputs myPredictions
  putStrLn $ alt4++"\n myAccuracy "++clr ++ show myAccuracy
 
  -- this is already handled, prob should rmv this
  -- I added calc.mean foreach myself
  let theLoss = fwdLossMean -- Lin.calcMean $ bckLoss

  -- BACK LOSS HERE IS ACTUALLY A FORWARD AND I NEED TO CALL A BACK ON THE FIRST FIRE ON BACKPROP
  -- ADDING [].append 2 to targets throws !!index too large error
  -- let theLoss =  Lin.calcMean [Activation.calcLoss' (fst ij) (snd ij) | ij <- zip (inputs lay02_actiSoft) testTargetsOneHot]
  
  putStrLn $ alt++" theLoss"++clr ++ show theLoss
  
  -- print $ [Activation.calcLoss' (fst ij) (snd ij) | ij <- zip (inputs lay02_actiSoft) testTargetsOneHot]
  -- print $ theLoss

  print $ length $ inputs lay01_actiSoft
  

  let returnLayer = Layer {
    inputs = inputs lay01_actiSoft,
    weights = weights lay01_actiSoft,
    biases = biases lay01_actiSoft
  }



  -- ##### STARTING BACKPROP ### @TODO - FIX THIS DOGSHIT
  -- NEED TO BACKPROP LOSS @TODO!!!!!!!
  -- need to be in the optimizer

  -- Add in backloss then begin permutation-based architecture planning

  -- DENSE LAYER BACKPROPAGATION 01
  -- @NOTE DOUBLE CHECK THE LAY INPUTS, I IMAGINE YOPU NEED THE FINISHED LAY - RLEU/Soft
  let dLayer01 = Layer {
    -- inputs = Lin.matDotPure dVals $ transpose <weights-from-dense-layer> 
    inputs = Lin.matDotPure (inputs returnLayer)  $ transpose (weights lay01),
    -- weights = Lin.matDotPure $ transpose <inputs-from-dense-layer>  dVals 
    weights = Lin.matDotPure (transpose $ inputs lay01)   (inputs returnLayer),
    biases = [sum <$> transpose (inputs returnLayer)]
  }
  -- putStrLn $ d_ylw ++ "\n dLayer01 " ++ clr ++ show dLayer01

  -- DENSE LAYER BACKPROPAGATION 00
  let dLayer00 = Layer {
    -- inputs = Lin.matDotPure dVals $ transpose <weights-from-dense-layer> 
    inputs = Lin.matDotPure (inputs lay00_actiRELU)  $ transpose (weights lay00_actiRELU),
    -- weights = Lin.matDotPure $ transpose <inputs-from-dense-layer>  dVals 
    weights = Lin.matDotPure (transpose $ inputs lay00_actiRELU)   (inputs lay00_actiRELU),
    biases = [sum <$> transpose (inputs lay00_actiRELU)]
  }
  -- putStrLn $ d_ylw ++ "\n dLayer00 " ++ clr ++ show dLayer00

-- @TODO THESE ARE NOT BEING USED THIS IS UNFINISHED AND I HAVENT OTOUCHED IN A BIT SO NEED TO FIX
  -- ALWAYS STARTS len(n[]) rather than n++ on backpropagation - so 01 then 00
  let backRELUProp = Activation.backRELU $  inputs dLayer01
  -- print $ backRELUProp
  -- -- already made Lin.backpropActiSoftFull
  let backActiSoftProp = Lin.backpropActiSoftFull <$> inputs dLayer01

  -- putStrLn $ alt2 ++ "  activationSoftmax backpropagation" ++ clr
  --   ++ show backActiSoftProp

  -- putStrLn $ d_ylw ++ "\n backRELUProp " ++ clr ++ show backRELUProp
  -- putStrLn $ d_ylw ++ "\n backActiSoftProp " ++ clr ++ show backActiSoftProp

  -- ##################################### BACK LOSS WILL GO HERE



    -- @NOTES  targets are    and backVals are 
    -- Double check if I am bringing these in proper, I need to label which is first
    -- let crossEnpBackLoss = crossEntropyBackLoss myPredictionsInputs myPredictions
    -- putStrLn $ alt4++"\n crossEnpBackLoss "++clr ++ show crossEnpBackLoss

    

  -- #####################################


  putStrLn $ alt++" 1: "++clr ++ show (weights dLayer01)
  putStrLn $ alt++" 2: "++clr ++ show (weights returnLayer)
 
  putStrLn $ alt++" 1: "++clr ++ show (weights dLayer00)
  putStrLn $ alt++" 2: "++clr ++ show (weights lay00_dot {--lay00_actiRELU--})



  -- ### OPTIMIZER 0.1.1 ###  T H E S E  A R E  M Y  O P T M I Z E R S ### BACKPROPAGATE ABOVE HERE ###

  -- fix this with conventions like used above
  -- iLyrUpdateWerights needs to pass a custiom multiplier
  --- bring everything into the lib and create functions which I can jsut call one name wiht one/two params and everything flies - custom typing after MVP
  --  @TODO - T H E S E  N E E D  T O  B E  R E D O N E !! @TODO
  {--
  
  - multiply each scalar val (these will be derivs) by -learningRate  from the _optimizer  to then take each val of the returned array and add it them to the actual layers passing through
  --}  
  let optimizerLayer00 = Layer {
    inputs = inputs returnLayer,
    -- multiplier - size - seed - dWeights
    weights = iLayerUpdateWeightsCstm (-learningRate _optimizer) ((length $ weights dLayer00), (length (head $ weights dLayer00))) mySeed (weights dLayer00),
    biases = iLayerUpdateBiasesCstm (-learningRate _optimizer)  ((length $ biases dLayer00), (length (head $ biases dLayer00))) mySeed (biases dLayer00)
  }
  let optimizerLayer01 = Layer {
    inputs = inputs returnLayer,
    -- multiplier - size - seed - dWeights
    weights = iLayerUpdateWeightsCstm (-learningRate _optimizer) ((length $ weights dLayer01), (length (head $ weights dLayer01))) mySeed (weights dLayer01),
    biases = iLayerUpdateBiasesCstm (-learningRate _optimizer)  ((length $ biases dLayer01), (length (head $ biases dLayer01))) mySeed (biases dLayer01)
  }

  -- let optimizerLayer02 = Layer {
  --   inputs = inputs returnLayer,
  --   -- multiplier - size - seed - dWeights
  --   weights = iLayerUpdateWeightsCstm (-learningRate _optimizer) ((length $ weights dLayer02), (length (head $ weights dLayer02))) mySeed (weights dLayer02),
  --   biases = iLayerUpdateBiasesCstm (-learningRate _optimizer)  ((length $ biases dLayer02), (length (head $ biases dLayer02))) mySeed (biases dLayer02)
  -- }

  -- #######################

   -- let trueL00OneHot =  Lin.calcMean [Activation.calcLoss' (fst ij) (snd ij) | ij <- zip (inputs lay02_actiSoft) testTargetsOneHot]
  -- let trueL01OneHot =  Lin.calcMean [Activation.calcLoss' (fst ij) (snd ij) | ij <- zip (inputs lay00) testTargetsOneHot]
  --let trueL00 =  Lin.calcMean [Activation.calcLoss' (fst ij) ([snd ij]) | ij <- zip (inputs lay02_actiSoft) testTargets]
  --let trueL01 =  Lin.calcMean [Activation.calcLoss' (fst ij) ([snd ij]) | ij <- zip (inputs lay00) testTargets]

  -- I think this is the more accurate loss, I need to double check later, as I wrote this second one for backpropagation Loss
  -- WRAP THIS IN A ONE HOT ENCODED IF NEED BE, PROB WILL NOT NEED TO WITH MY CURR HACK
  let nowLoss = if oneHotEncodedTargets then Lin.calcMean $ Activation.fwdLossOneHot (int2Double testTargetsOneHot) (inputs lay01_actiSoft) else Lin.calcMean $ Activation.fwdLoss testTargets (inputs lay01_actiSoft)
  let startLoss = if firstEpoch then nowLoss else (bestLoss _optimizer)
  
  putStrLn $ alt++" nowLoss: "++clr ++ show nowLoss

  -- Lin.calcMean $ Activation.fwdLossOneHot (int2Double testTargetsOneHot) (inputs lay00)
  -- let l00OneHot = Lin.calcMean $ Activation.fwdLossOneHot (int2Double testTargetsOneHot) (inputs lay02_actiSoft)
  -- let l01OneHot = Lin.calcMean $ Activation.fwdLossOneHot (int2Double testTargetsOneHot) (inputs lay00)
  -- let l00 = Lin.calcMean $ Activation.fwdLoss testTargets (inputs lay02_actiSoft)
  -- --let l01 = Lin.calcMean $ Activation.fwdLoss (testTargets) (inputs lay00)
  -- let l01 = if firstEpoch then Lin.calcMean $ Activation.fwdLoss testTargets (inputs lay00) else bestLoss
  -- -- let betterLoss = False

  -- -- trueL01 and trueL02 are crossEntropy loss I am pretty sure - i need to check - fwd is correct for now
  -- --let betterLoss = if trueL01 > trueL00 then True else False
  -- -- THIS WAY IS CORRECT WITH GT AND LT
  -- -- BELOW IS THE RIGHT WAY IN CASE IT GETS COMMENTED OUT
  -- let betterLoss = if l00 < l01 then True else False
  let betterLoss = nowLoss <= startLoss
  let bestLoss' = if betterLoss then nowLoss else (bestLoss _optimizer)
  -- --   print $ betterLoss

  -- make return layer become the last pass of actiSoft for your layer?
  -- double check the return layer pass everytime


  --- ######################### MIGHT NEED TO CHECK IF THIS GOES BELOW OR ABOVE THE OPTIMIZER.
  -- print optimizerLayer

  -- putStrLn $ show nowLoss ++" LT "++show startLoss
  
  -- if betterLoss /= True then putStrLn $ b_red++show betterLoss else putStrLn $  b_green++show betterLoss
  -- putStrLn $ "\n" ++ b_cyan ++ "BEST LOSS: " ++ clr ++ show bestLoss' ++ "\n"

  if betterLoss then putStrLn $ b_green++"\n BETTER LOSS THIS EPOCH - returnLayer "++clr++show returnLayer else putStrLn $ b_red++"WORSE LOSS THIS EPOCH"++clr

  -- print $ bestLoss'
  putStrLn $ b_black++"________________________________________________________________________\n\n"++clr

  -- print $ returnLayer
  -- putStrLn $ "biases returnLayer " ++ show (biases returnLayer)

  let returnOptimizer' = IOptimizer {
    currEpoch = (currEpoch _optimizer) + 1,
    learningRate = learningRate iOptimizer,
    bestLoss = bestLoss',
    -- THESE ARE NOT THE BEST LAYERS AND THEY ARE NOT ACTUALLY RIGGED UP!!!!!!
    bestLayers = if betterLoss then [optimizerLayer00, optimizerLayer01] else bestLayers _optimizer,
    layerSize = lSize
  }

  putStrLn $ d_ylw ++ "  EPOCH(" ++ b_black ++ show (currEpoch returnOptimizer') ++ d_ylw ++ ") "++clr
    ++ b_cyan ++ "\n  BEST LOSS: " ++ clr ++show (bestLoss returnOptimizer')  ++ "\n"
    ++ d_ylw ++ "\n  NET SIZE: " ++ clr ++show (layerSize returnOptimizer')  ++ "\n"
  
  -- @TODO optimizer updates these nad that becomes the new layers to throw in
  let layersRanThroughNet = INet{
    -- THIS NEEDS TO BE FIXED AND PASSED THROUHG A WOKRING OPTIMIZER. ABOVE IS NOT WHAT I WANT
    allLayers = [lay00_actiRELU, lay01_actiSoft]}

  let firstEpoch = False

  -- let progress = fromIntegral (currEpoch _optimizer) / fromIntegral epochs
  --  putStrLn $ "%"++ show progress 

  loopNTimes (n-1) firstEpoch betterLoss layersRanThroughNet returnOptimizer'


-- ################# ABOVE IS RECURSIVE NET LOOP ###########################
-- ################# ABOVE IS RECURSIVE NET LOOP ###########################
-- ################# ABOVE IS RECURSIVE NET LOOP ###########################
-- ################# ABOVE IS RECURSIVE NET LOOP ###########################


{- @TODO rmv numOFInputs and grab dynamically on a where after I pull in an xs, this will be used to gen my rand weights mxn/list} -}
{- params are flipped for an mxn -}
-- initDenseLayer inputs' = do 
--   let numOfInputs' = length $ head inputs'
--   let numOfNeurons' = length inputs'
 
--   putStrLn ("\ESC[0;1;38mRows "++show numOfNeurons')
--   putStrLn ("Cols " ++show numOfInputs'++C.clr)
--   -- @TODO this is going to be a fucntional "while", they, online, cypherturds , say to use recursion online but that feels <$@&*$> in the main loop
--   newRandSeed <- Lin.randSeed
--   let randWeightsForLayer = Lin.randLayer numOfInputs' numOfNeurons' (newRandSeed-1)
--   print randWeightsForLayer

--   let returnLayer = Layer {
--     inputs = inputs',
--     weights = randWeightsForLayer,
--     biases = [Lin.zeroList numOfInputs']
--   }

--   putStrLn ("\n\n"++C.d_ylw++" initDenseLayer - returnLayer"++C.clr)
--   print returnLayer

main = do
  putStrLn (C.alt2++"\n\n Hello, my name is Acropolis"++C.clr)
  print $ Net.isRunning True
  
  putStrLn (C.alt2++"\n oneHotEncodedTargets "++C.clr)
  print $ oneHotEncodedTargets
  
  mySeed <- Lin.randSeed
  putStrLn ("\n"++C.b_cyan++"randSeed: "++show mySeed++C.clr)
  
  -- let randWeights = Lin.randLayer 3 3 mySeed
  -- let myMatDot = transpose (Lin.matDot mockI2 randWeights (zeroList 3))


  -- putStrLn ("\n"++C.b_cyan++"---------"++C.clr++"\n "++C.d_ylw++"inputs"++clr)
  -- print mockI2
  -- putStrLn ("\n"++C.d_ylw++" weights"++C.clr)
  -- print randWeights
  -- putStrLn ("\n"++C.d_ylw++" myMatDot"++C.clr)
  -- print myMatDot
  
  -- {- Euler's Num: 2.718281828459045
  --  -- functor example
  --  let xc = [4.8, 1.21, 2.385]  >>= \x -> [2.718281828459045**x] >>= \x -> [x/100]
  --  print xc
  -- -}
  
  -- let inputsAfterRELU = Activation.actiRELU <$> myMatDot
  -- putStrLn ("\n"++C.d_ylw++" inputsAfterRELU"++C.clr)
  -- print inputsAfterRELU
  
  -- let actiSoft' = Activation.actiSoft <$> inputsAfterRELU
  -- putStrLn ("\n"++C.d_ylw++" actiSoft xs"++C.clr)
  -- print $ actiSoft'
  
  -- putStrLn ("\n"++C.d_ylw++" sum <$> actiSoft"++C.clr)
  -- print (sum <$> (Activation.actiSoft <$> inputsAfterRELU))
  -- putStrLn "\n\n"

  -- let ccc = Activation.fwdLoss testTargets actiSoft'
  -- print $ ccc
  -- print $ testTargets


  -- let randWeightsForLayer2 = randLayer 3 11 (mySeed+69)
  -- {- @OTOD - on init we can use zeros with the lenght being numOfNeurons, I think, double check puta -}
  -- -- let bias_11 = [0.2, 0.4, 0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.4, 0.6, 0.9]
  -- let bias_11 = zeroList 11 
  
  -- {- @TODO - Use . or $ and go from right to left on a refactor when bored -}
  -- --  let denseLayer2 = transpose (Lin.matDot (map Activation.actiSoft inputsAfterRELU) randWeightsForLayer2 bias_11)
 
  -- let denseLayer2 = transpose $ Lin.matDot (Activation.actiSoft <$> inputsAfterRELU) randWeightsForLayer2 bias_11
  -- putStrLn ("\n"++C.d_ylw++" DenseLayer2 \n"++clr)
  -- print $ denseLayer2 
  -- -- print $  randLayer 88 2 (mySeed+3)
  -- {- SKIPPING LAYER FOR EXAMPLE -}
  -- print $ (Activation.actiSoft <$> denseLayer2)

  -- putStrLn ("\n\n"++C.d_ylw++" initDenseLayer 2 3 *"++C.clr)

  {- cols by rows NOT rows by cols -}
  {- this is actually a 2x3 -}
  -- THIS IS A DO BLOCK
  -- initDenseLayer mockI2

  -- let initializedLayer = iDLayerFromInputs mockI2 mySeed
  {-
   - let lay00 = iDense 2 3 (mySeed+53464)
   - let lay01 = iDense 3 6 (mySeed+32342)
   - let lay02 = iDense 6 2 (mySeed+25354)
  -}
 
  {- @NOTE |  flipping all mxn to nxm |  IMPORTANT !! -}
  -- was 2x2 but it was causing a 2x2 actiSoft, odd bug I think I am ,utlplyiong my doyt backwards as I sould, need to double check
  let lay00 = iDLayerFromInputs testInputs (mySeed+5280)
  let iNet'' = INet {allLayers = [lay00]}

  -- let fff = Lin.matDotPure [[1,2,3],[4,5,6]] (transpose [[1,2],[3,4],[5,6]])
  -- print $ fff
  -- @Todo - make a matDotPure function without biases added
  -- BACK PROP DENSE LAYER
  -- let dLayer = Layer {
  --   -- inputs = Lin.matDotPure dVals $ transpose <weights-from-dense-layer> 
  --   inputs = Lin.matDotPure [[0.1,0.2,0.3],[0.4,0.5,0.6]]   $ transpose [[0.1,0.2],[0.3,0.4],[0.5,0.6]],
  --   -- weights = Lin.matDotPure $ transpose <inputs-from-dense-layer>  dVals 
  --   weights = Lin.matDotPure (transpose [[0.1,0.2],[0.3,0.4],[0.5,0.6]])   [[0.1,0.2,0.3],[0.4,0.5,0.6]],
  --   biases = [sum <$> transpose [[0.1,0.2,0.3], [0.4,0.5,0.6]] ] -- dVals
  -- }  print $ dLayer
  -- I HAVE A WAY TO LOOP 1-n instead of reverse
  loopNTimes epochs True False iNet'' iOptimizer

  putStrLn $ b_black++"________________________________________________________________________\n\n"++clr

  putStrLn $ b_black++"  ## EPOCHS: "++show epochs++clr ++
    b_black++"  ## ONE HOT ENCODED TARGETS: "++show oneHotEncodedTargets++clr ++
    b_black++"\n  ## T R A I N I N G  C O M P L E T E ##\n\n"++clr

  -- let backRLTest = Activation.backRELU $ inputs lay00
  -- print $ backRLTest

  -- print $ dataZipped
  --  print $ [ fst i | i <- dataZipped]

-- return each [] as scalar of targetVal[index]  so [0 ,2, 4]  [ 2] == 4 here as 4 is index 2  (0, 1, 2)
-- check if it is [[]] or [] on targetVals to then apply properly, it changes a tad when multidim
-- this crunches the actiSoftmax back into the orig shape from the fizz'd out sum of 1

-- @TODO finish opti for better preds
