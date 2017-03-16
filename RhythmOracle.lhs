Learning Draft
Donya Quick

> module RhythmOracle where
> import Kulitta
> import Kulitta.Learning.InsideOutside
> import System.Random

> data X = X 
>     deriving (Eq, Show)

> rSet :: [Rule X Double] -- a regular rule set
> rSet = [
>     (X, 0.5)  :-> \t -> [NT(X, t/2), NT(X, t/2)],
>     (X, 0.25) :-> \t -> [NT(X, 3*t/4), NT(X, t/4)],
>     (X, 0.25) :-> \t -> [NT(X, 7*t/8), NT(X, t/8)]]

> rSetI :: [TRuleInst (X, Double)] -- same concepts as rule instances with total duration 1.0
> rSetI = [
>     TRuleInst 0 (X,1.0) [(X, 1/2), (X, 1/2)],
>     TRuleInst 1 (X,1.0) [(X, 3/4), (X, 1/4)],
>     TRuleInst 2 (X,1.0) [(X, 7/8), (X, 1/8)]]

> sumDurs :: [(X,Double)] -> Double
> sumDurs = sum . map snd

> compareFun = (==) -- may need to make this error tolerant later for tiny durs

Possible factors: 1.0, 1.5, 1.25

> findByRHSX :: [(X, Double)] -> [TRuleInst (X, Double)]
> findByRHSX seq = 
>     let dSum = sumDurs seq -- how much total duration does the sequence occupy?
>         normSeq = map (\(x,p) -> (x,p/dSum)) -- normalize to a total duration of 1.0
>         unNormSeq = map (\(x,p) -> (x,dSum * p)) -- un-normalize to occupy dSum of duration
>         seqN = normSeq seq -- normalized version of seq
>         matchFun tr = compareFun seqN (rhs tr) -- function to check for matching rule instances
>         tris = filter matchFun rSetI -- fetch matching rule instances (each with total dur 1.0)
>         unNormTRI (TRuleInst i (x,p) rhs) = TRuleInst i (x,dSum) (unNormSeq rhs) -- un-norm function for instances
>     in  map unNormTRI tris  -- un-normed matching rule instances

> testSeqs = map f [0,1,2,3,4,5] where
>     f seed = toPairs $ snd (gen rSet (mkStdGen seed, [NT(X,4.0)]) !! 4 )

> rOracle =  TOracle [0.3, 0.3, 0.4] (X,4.0) [[0,1,2]] findByRHSX

Format: learnProbs oracle trainingData iterations minimumProb convergenceThreshold

> testLearning = learnProbs rOracle testSeqs 100 0.01 0.02

=================================

Removed stuff

isPower2 :: (RealFrac a, Floating a) => a -> Bool
isPower2 x = let y = logBase 2 x in y == fromIntegral (round y)

isPower2Fac :: (RealFrac a, Floating a) => a -> a -> Bool
isPower2Fac factor x = isPower2 (x / factor)

