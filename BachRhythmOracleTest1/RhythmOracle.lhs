> module RhythmOracle where
> import Rhythm
> import Kulitta
> import Euterpea
> import Kulitta.Learning.InsideOutside
> import System.Random
> import Data.List

> rSet :: [Rule RTerm Double] -- a regular rule set
> rSet = normalize [
>     (Beat, 0.2)  :-> \t -> [NT(Beat, t/2), NT(Beat, t/2)],
>     (Beat, 0.3)  :-> \t -> [NT(Beat, t/2), NT(Beat, t/4), NT(Beat, t/4)],
>     (Beat, 0.0)  :-> \t -> [NT(Beat, t/4), NT(Beat, t/2), NT(Beat, t/4)],
>     (Beat, 0.0)  :-> \t -> [NT(Beat, t/4), NT(Beat, t/4), NT(Beat, t/2)],
>     (Beat, 0.3)  :-> \t -> [NT(Dotted, 3*t/4), NT(Beat, t/4)],
>     (Beat, 0.0)  :-> \t -> [NT(Beat, t/4), NT(Dotted, 3*t/4)],
>     (Beat, 0.1)  :-> \t -> [NT(Beat, t/4), NT(Beat, t/4), NT(Beat, t/4), NT(Beat, t/4)],
>     (Beat, 0.1)  :-> \t -> [NT(Beat, t/3), NT(Beat, t/3), NT(Beat, t/3)]
>     ]

> getProbs :: [Rule a b] -> [Double]
> getProbs = map fp where
>     fp ((x,p) :-> _ ) = p

> rSetI :: [TRuleInst (RTerm, Double)] -- same concepts as rule instances with total duration 1.0
> rSetI = [
>     TRuleInst 0 (Beat,1.0) [(Beat, 1/2), (Beat, 1/2)],
>     TRuleInst 1 (Beat,1.0) [(Beat, 1/2), (Beat, 1/4), (Beat, 1/4)],
>     TRuleInst 2 (Beat,1.0) [(Beat, 1/4), (Beat, 1/2), (Beat, 1/4)],
>     TRuleInst 3 (Beat,1.0) [(Beat, 1/4), (Beat, 1/4), (Beat, 1/2)],
>     TRuleInst 4 (Beat,1.0) [(Dotted, 3/4), (Beat, 1/4)],
>     TRuleInst 5 (Beat,1.0) [(Beat, 1/4), (Dotted, 3/4)],
>     TRuleInst 6 (Beat,1.0) [(Beat, 1/4), (Beat, 1/4), (Beat, 1/4), (Beat, 1/4)],
>     TRuleInst 7 (Beat,1.0) [(Beat, 1/3), (Beat, 1/3), (Beat, 1/3)]]

> sumDurs :: [(a,Double)] -> Double
> sumDurs = sum . map snd

> compareFun = (==)

> findByRHSX :: [(RTerm, Double)] -> [TRuleInst (RTerm, Double)]
> findByRHSX seq = 
>     let dSum = sumDurs seq -- how much total duration does the sequence occupy?
>         normSeq = map (\(x,p) -> (x,p/dSum)) -- normalize to a total duration of 1.0
>         unNormSeq = map (\(x,p) -> (x,dSum * p)) -- un-normalize to occupy dSum of duration
>         seqN = normSeq seq -- normalized version of seq
>         matchFun tr = compareFun seqN (rhs tr) -- function to check for matching rule instances
>         tris = filter matchFun rSetI -- fetch matching rule instances (each with total dur 1.0)
>         unNormTRI (TRuleInst i (x,p) rhs) = TRuleInst i (x,dSum) (unNormSeq rhs) -- un-norm function for instances
>     in  map unNormTRI tris  -- un-normed matching rule instances

=========================================

> testSeqs = map f [0,1,2,3,4,5] where
>     f seed = toPairs $ snd (gen rSet (mkStdGen seed, [NT(Beat,4.0)]) !! 4 )

> initProbs = take 8 $ repeat (1/8)
> rOracle =  TOracle initProbs (Beat,4.0) [[0..7]] findByRHSX

Format: learnProbs oracle trainingData iterations minimumProb convergenceThreshold

> testLearning = learnProbs rOracle testSeqs 100 0.01 0.02

=========================================

> bachCantataSeqsRaw = [ -- from BWV 4 markers 100 and 103
>     [qn, den, sn, qn, qn],
>     [qn, qn, hn],
>     [dqn, en, en, sn, sn, en, en],
>     [qn, dqn, en, en, en],
>     [qn, en, en, en, sn, sn, en, en],
>     [en, en, en, en, sn, sn, sn, sn, qn],
>     [en, en, en, en, sn, sn, sn, sn, en, en],
>     [en, en, en, en, en, en, en, en],
>     [en, en, den, sn, sn, sn, sn, sn, en, en]
>     ]

> bachCantataSeqs = map (map toBeat) bachCantataSeqsRaw

> toBeat x =
>     if x == den then (Dotted, fromRational den)
>     else if x == dqn then (Dotted, fromRational dqn) else (Beat, fromRational x)

> rOracle2 =  TOracle initProbs (Beat,1.0) [[0..7]] findByRHSX
> testLearning2 = do
>     ps <- learnProbs rOracle2 bachCantataSeqs 100 0.01 0.02
>     writeFile "bachCantataProbs.txt" (concat $ intersperse "\n\n" $ map show ps)

> bachProbs :: [Double]
> bachProbs = 
>     read "[0.8510911736306143, 4.426342216253392e-3, 2.9856772830154275e-2, 2.3793216219438296e-3, 7.110892668979364e-2, 1.0e-2, 1.0e-2, 1.0e-2]"

> bachRules = updateProbs rSet bachProbs

> bachGenTests i g = 
>     let (g2,x) = (gen bachRules (g, [NT(Beat,1.0)]) !! i )
>     in  map snd (toPairs x) : bachGenTests i g2

> resultsToMusic [] = rest 0
> resultsToMusic (x:xs) = line (map (c 4 . toRational) x) :+: resultsToMusic xs

> bachOutput1 = resultsToMusic $ take 8 $ bachGenTests 3 (mkStdGen 1234)
> bachInput = resultsToMusic bachCantataSeqsRaw


