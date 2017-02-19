> module Final where
> import Euterpea
> import Kulitta.PTGG
> import System.Random
> import Data.List
> import Data.Fixed

Brian Heim
blh33

CPSC 431/531 Final Project
Application of musical grammars (PTGG) to rhythm.

---------------------------------
-----------DEFINITIONS-----------

Maximum power of two used as a subdivision (i.e. maxPoT = 4 => 1/(2^4) = 1/16 is smallest unit in a measure)

> maxPoT = 4

The main symbol is `Beat`, which is any even subdivision of a measure (half, quarter, and eighth notes, for example)
`Dotted` is a Beat with 3/2 the duration
`QuarterDotted` is a Beat with 5/4 the duration
`Short` is a Beat which has reached the maximum power of two and cannot be further subdivided (convenience)

> data RTerm = Measure | 
>              FourFour | ThreeFour | TwoFour | SixEight | NineEight | 
>              FiveEight |
>              --CustomMeasure Int Int |
>              Beat | Dotted | Short | QuarterDotted
>     deriving (Eq, Ord, Enum, Read, Show)

> allRTerms = [Measure, FourFour, ThreeFour, TwoFour, SixEight, NineEight, FiveEight, Beat, Dotted, Short, QuarterDotted]

-------------------------------------------
-----------PARAMETER DEFINITIONS-----------

Three parameters:
  power and ratio: the duration of the beat is 1/2^power*ratio. Ratio is usually 1, unless the
    Beat is within a tuplet in which case it becomes <written number of beats in tuplet>/<actual duration of tuplet>
    Thus a triplet has ratio 2/3, a quintuplet ratio 4/5
  measures: determines the length of a Measure object during the initial phase of generation and is not used for any
  final calculations (should always end up =1)

> data Param = Param {power :: Int, measures :: Int, ratio :: Rational}
>     deriving (Eq, Show)

Default parameter is a single whole note, of one full measure, with a normal ratio

> defParam = Param 0 1 1

Modifier functions:

> powFcn x p = p{power = power p + x}
> half = powFcn 1
> quarter = powFcn 2
> eighth = powFcn 3

> maxPow :: Param -> Bool
> maxPow p = power p == maxPoT

> toMaxPow :: Param -> Int
> toMaxPow p = maxPoT - power p

> mkRatio r p = p{ratio = ratio p * r}

> halveMeasures p = p{measures = (measures p) `div` 2}

Convenience method: get the actual duration of p

> calcDur :: RTerm -> Param -> Rational
> calcDur t p = 2^(maxPoT-power p)*l*(ratio p) where
>                       l = case t of
>                           Dotted -> 1.5
>                           QuarterDotted -> 1.25
>                           _ -> 1.0

Convenience method: returns Short if the subdivision about to be used will make the new notes the shortest possible

> shortIfMaxed x p = if toMaxPow p == x then Short else Beat

===========CONVENIENCE METHODS===========

> log2Floor = truncate . logBase 2

Returns the highest power of two not greater than the argument

> potFloor :: Int -> Double
> potFloor x = 2.0^(log2Floor $ fromIntegral x)

subdivide used to do something different, it now simply redirects to subdivN for convenience and in case it needs to be
expanded in the future.

> subdivide :: Param -> [Int] -> [Term RTerm Param]
> subdivide p xs = subdivN p xs

subdivN is a convenience method that, given a Term's Param and a list of Ints, returns a list of Beats, Shorts, and Dotteds
that represent dividing the parent Beat (whose Param is passed in) into smaller units, where a `1` in the list ends up with
duration <Beat's duration>/<sum of the list>. In this way, any arbitrary tuplet or non-tuplet subdivision may be easily
and concisely written without specifying anything other than the durations of its components. For example,
subdivN p [1,1] divides a beat in half; subdivN p [1,1,1] divides it into a triplet; subdivN p [2,3] makes a quintuplet
with the first note 2/5 the duration of the Beat and the second note 3/5 the duration. Any subdivisions which would result
in a written note smaller than the maxPoT will return (Beat, p). However, actual durations smaller than 1/2^maxPoT (for
example, triplet 16ths) may result.

> subdivN :: Param -> [Int] -> [Term RTerm Param]
> subdivN p xs = if toMaxPow p < pwr then [NT (Beat, p)] else map f xs where
>                   s = fromIntegral $ sum xs
>                   r = (toRational $ potFloor s) / (fromIntegral s)
>                   pwr = log2Floor $ potFloor s
>                   f n = NT (rterm n, mkRatio r $ powFcn (log2Floor (potFloor s / potFloor n)) p) where
>                         rterm x = case (fromIntegral x)/(potFloor x) of
>                                        1.0 -> shortIfMaxed (pwr `div` n) p
>                                        1.5 -> Dotted
>                                        1.25 -> QuarterDotted
>                                        _ -> error "subdivN: check the array; there is an invalid value"

---------------------------
-----------RULES-----------

The rules for `measure generation` determine what measures will be tied together. `letChance` determines how likely this is to happen.

> mRules :: Prob -> [Rule RTerm Param]
> mRules letChance = if (letChance < 0.0) || (letChance > 1.0) then error "mRules: Chance is not within 0.0-1.0" else [
>     (Measure, 1-letChance) :-> \p -> if (measures p > 1) then [NT (Measure, halveMeasures p), NT (Measure, halveMeasures p)] else [NT (Measure, p)],
>     (Measure, letChance) :-> \p -> if (measures p > 1) then [Let "x" [NT (Measure, halveMeasures p)] [Var "x", Var "x"]] else [NT (Measure, p)]]

Rules for time signature generation. Assumes max PoT is not violated here.

> tRules :: Bool -> [Rule RTerm Param]
> tRules useLets = normalize ([
>   (Measure, 1.0) :-> \p -> [NT (ThreeFour, p)], -- 3/4
>   (Measure, 0.0) :-> \p -> [NT (NineEight, p)], -- 9/8
>   (Measure, 0.0) :-> \p -> [NT (FiveEight, p)] -- 5/8
>   ] ++ if useLets then letRules else []) where
>       letRules = [
>   --let rules go here
>           ]

Rules for turning time signatures into beat patterns

> bRules :: Bool -> [Rule RTerm Param]
> bRules useLets = normalize ([
>   (ThreeFour, 1.0) :-> \p -> [NT (Beat, quarter p), NT (Beat, quarter p), NT (Beat, quarter p)],
>   (ThreeFour, 0.0) :-> \p -> [NT (Beat, quarter p), NT (Beat, half p)],
>   (ThreeFour, 2.0) :-> \p -> [NT (Beat, half p), NT (Beat, quarter p)],
>   (NineEight, 1.0) :-> \p -> [NT (Dotted, quarter p), NT (Dotted, quarter p), NT (Dotted, quarter p)],
>   (FiveEight, 1.0) :-> \p -> [NT (Dotted, quarter p), NT (Beat, quarter p)]
>   --(NineEight, 1.0) :-> \p -> [NT (Dotted, half p), NT (Dotted, quarter p)]
>   ] ++ if useLets then letRules else []) where
>       letRules = [
>           (ThreeFour, 1.0) :-> \p -> [Let "x" [NT (Beat, quarter p)] [NT (Beat, quarter p), Var "x", Var "x"]],
>           (ThreeFour, 0.0) :-> \p -> [Let "x" [NT (Beat, quarter p)] [Var "x", Var "x", NT (Beat, quarter p)]],
>           (NineEight, 1.0) :-> \p -> [Let "x" [NT (Dotted, quarter p)] [Var "x", Var "x", Var "x"]],
>           (NineEight, 1.0) :-> \p -> [Let "x" [NT (Dotted, quarter p)] [NT (Dotted, quarter p), Var "x", Var "x"]]
>   --let rules go here
>           ]

Rules for rhythmic subdivision.

> rRules :: Bool -> [Rule RTerm Param]
> rRules useLets = normalize ([
>    --modes of subdividing beats:
>    --unchanged
>   (Beat, 1.0) :-> \p -> [NT (Beat, p)],
>      --half and half
>   (Beat, 0.15) :-> \p -> subdivide p [2,1,1],
>   (Beat, 0.05) :-> \p -> subdivide p [1,1,1,1],
>   --dotted half + quarter
>   (Beat, 0.1) :-> \p -> subdivide p [3,1],
>   --half, quarter, quarter
>   (Beat, 0.1) :-> \p -> subdivide p [1,1],
>   --syncopation
>   --(Beat, 0.05) :-> \p -> subdivide p [1,2,1],
>   --(Beat, 0.02) :-> \p -> subdivide p [1,1,1],
>      --triplet (disabled because of duplicate in lets)
>   (Beat, 0.1) :-> \p -> subdivide p [1,1,1],
>      --quintuplet (disabled because of stylistic distance)
>      --(Beat, 0) :-> \p -> subdivide p [1,1,1,1,1],
>   --keep a short short, a dotted dotted
>   (Short, 1.0) :-> \p -> [NT (Short, p)],
>   (Dotted, 3.0) :-> \p -> [NT (Dotted, p)],
>   (Dotted, 1.0) :-> \p -> [NT (Beat, p), NT (shortIfMaxed 1 p, half p)], -- subdivide [2,1]
>   (Dotted, 1.0) :-> \p -> [NT (shortIfMaxed 1 p, half p), NT (shortIfMaxed 1 p, half p), NT (shortIfMaxed 1 p, half p)], -- subdivide [1,1,1]
>   (QuarterDotted, 1.0) :-> \p -> if toMaxPow p < 1
>                                  then [NT (Beat, p)]
>                                  else [NT (Dotted, half p), NT (Beat, half p)] -- subdivide [3,2]
>   ] ++ if useLets then letRules else []) where
>       letRules = [
>           --let rules are [x=1,x=1], [x=1,2,x=1], [x=1,x=1,x=1] ~ symmetric halves, thirds, and syncopation with symmetric bookends
>           (Beat, 0.1) :-> \p -> [Let "x" [NT(shortIfMaxed 1 p, half p)] [Var "x", Var "x"]],
>           (Dotted, 1.0) :-> \p -> [Let "x" [NT(shortIfMaxed 1 p, half p)] [NT (shortIfMaxed 1 p, half p), Var "x", Var "x"]], -- subdivide [1,x,x]
>           (Dotted, 1.0) :-> \p -> [Let "x" [NT(shortIfMaxed 1 p, half p)] [Var "x", Var "x", Var "x"]] -- subdivide [x,x,x]
>           --(Beat, 0.1) :-> \p -> if toMaxPow p < 2 then [NT (Beat, p)] else
>           --    [Let "x" [NT(shortIfMaxed 2 p, quarter p)] [Var "x", NT(Short, half p), Var "x"]],
>           --(Beat, 0.05) :-> \p -> if toMaxPow p < 2 then [NT (Beat, p)] else
>           --    [Let "x" [NT(shortIfMaxed 2 p, quarter p)] [Var "x", Var "x", Var "x", Var "x"]],
>           --(Beat, 0) :-> \p -> [Let "x" [NT(shortIfMaxed 1 p, half $ mkRatio (2/3) p)] [Var "x", Var "x", Var "x"]]
>           ]

--------------------------------
-----------GENERATION-----------

Generation: fullGen s i m : s is the gen seed, i is the iteration to draw from, m is the length in measures (must be a multiple of two)

> mGen :: Int -> Int -> Int -> Double -> Sentence RTerm Param
> mGen s i m p = snd $ gen (mRules p) (mkStdGen s, [NT (Measure, Param 0 m 1)]) !! i

> tGen :: Int -> Bool -> Sentence RTerm Param -> Sentence RTerm Param
> tGen s ul terms = snd $ gen (tRules ul) (mkStdGen s, terms) !! 1 -- only need 1 cycle for time sig conversion

> bGen :: Int -> Bool -> Sentence RTerm Param -> Sentence RTerm Param
> bGen s ul terms = snd $ gen (bRules ul) (mkStdGen s, terms) !! 1 -- only need 1 cycle for beat pattern conversion

> rGen :: Int -> Bool -> Int -> Sentence RTerm Param -> Sentence RTerm Param
> rGen s ul i terms = snd $ gen (rRules ul) (mkStdGen s, terms) !! i

> --Pass through all phases with some assumptions (seed and useLets are global, m is power-of-two)
> fullGen :: Int -> Int -> Double -> Bool -> Int -> [(RTerm, Param)]
> fullGen s m mp ul ri = toPairs $ rGen s ul ri $ bGen s ul $ tGen s ul $ mGen s mi m mp where
>                           mi = log2Floor $ fromIntegral m

---------------------------------
-------------PLAYBACK------------

> addFinalBar :: [(RTerm, Param)] -> [(RTerm, Param)]
> addFinalBar xs = xs ++ [(Beat, Param 0 1 1)]

> transform :: [(RTerm, Param)] -> Music Pitch
> transform [] = rest 0
> transform xs =
>     let f (t, Param pow _ r) = note (0.5^pow*(modifier t)*r) (pitch 60) where
>         modifier x = case x of
>              Dotted -> 1.5
>              _ -> 1.0
>     in instrument Percussion $ foldr (:+:) (rest 0) $ map f xs

> addClick :: Music Pitch -> Music (Pitch, Volume)
> addClick m = addVolume 127 m /=: clicktrack where
>              clicktrack = addVolume 60 $ instrument Percussion $ foldr (:+:) (rest 0) $ map (\x -> note qn (pitch x)) clicks
>              clicks = (concat $ take (truncate $ dur m - 1) $ repeat [64,69,69,69]) ++ [64]

Convenience: click track + final whole note

> transform' = addClick . transform . addFinalBar
> playclick = play . transform'
> playraw = play . transform . addFinalBar -- without click track

Demo functions to try out different combinations of seed and iteration

> demoN s i n = play $ transform' $ fullGen s n 0 True i
> demo2 s i = demoN s i 2
> demo4 s i = demoN s i 4
> demo8 s i = demoN s i 8

---------------------------------
----------PRESENTATION-----------

Attempt at a cleaner list presentation (doesn't work especially well, would be nice to separate things by measure)

> present :: [(RTerm, Param)] -> String
> present [] = ""
> present ((t, p):xs) = f t p ++ delim xs "  " ++ present xs where
>                       f t p = show $ calcDur t p

> showRules :: [Rule RTerm Param] -> String
> showRules [] = ""
> showRules (r:rs) = show (round $ (*1000) $ prob r) ++ delim rs "," ++ showRules rs

> delim :: [a] -> String -> String
> delim [] s = ""
> delim _ s = s

---------------------------------
-----------EXPERIMENTS-----------

Generation experiments: playing around with updateProbs

> probs n g = map (\x -> (fromIntegral (x `mod` 1000) / 1000)^4) $ take n $ list n g where
>                 list 0 _ = []
>                 list x g = let (a,s) = next g in a : list (x-1) s

> randomRules s = let rs = rRules True
>                     ps = probs (length rs) (mkStdGen s) in
>                       normalize $ updateProbs rs ps

This is the most convenient method to use here: s1 and s2 are the seeds for rules and generation respectively.
i, m, and p are the index, length in measures, and Let-probability during measure gen

> randomRulesGen s1 s2 i m p = toPairs $ snd $ gen (randomRules s1) ms !! i where
>                              ms = gen (mRules p) (mkStdGen s2, [NT (Measure, Param 0 m 1)]) !! (log2Floor $ fromIntegral m)

