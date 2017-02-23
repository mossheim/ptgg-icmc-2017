Onset preserving expansion function for PTGGs
Donya Quick

> module Expand where
> import Kulitta
> import Kulitta.Grammars.MusicGrammars
> import System.Random
> import Data.List

Calculate the total duration of an expanded Sentence.

> totalDur :: Sentence a MP -> Dur
> totalDur [] = 0
> totalDur (t:ts) = case t of
>     NT (a,b) -> dur b + totalDur ts
>     x -> error ("Unsupported expression (totalDur)")

Shift all onsets in a Sentence by a constant amount.

> shiftOnsets :: Dur -> Sentence a MP -> Sentence a MP
> shiftOnsets d [] = []
> shiftOnsets d (t:ts) = case t of
>     NT (x,p) -> NT (x, p{onset = onset p + d}) : shiftOnsets d ts
>     x -> error ("Unsupported expression (shiftOnsets)")

Expand a Sentence while adjusting onsets of Let statements. 

> expand2 :: Dur -> [(String, Sentence a MP)] -> Sentence a MP -> Sentence a MP
> expand2 d e [] = []
> expand2 d e (t:ts) = case t of 
>     Let x a exp -> 
>         let vx = expand2 d e a
>             v = expand2 d ((x, shiftOnsets (-d) vx) : e) exp
>         in  v ++ expand2 (d+totalDur v) e ts
>     Var x -> 
>         let v = (maybe (error (x ++ " is undefined")) id $ lookup x e)
>         in  shiftOnsets d v ++ expand2 (d+totalDur v) e ts
>     NT(x,p) -> NT(x,p{onset=d}) : expand2 (d+dur p) e ts

================================================
Testing

> data X = X
>     deriving (Eq, Show)

> rules :: [Rule X MP]
> rules = [
>     (X, 0.4) :-> \p -> [NT (X, h p), NT (X, ho p)],
>     (X, 0.4) :-> \p -> [NT (X,p)],
>     (X, 0.2) :-> \p -> [Let "x" [NT(X, (h p){onset=0})] [Var "x", Var "x"]] -- onset has to be zero
>     ]

> ss :: [Term X MP]
> ss = [NT(X,defMP{dur = 16})]
> foo = snd $ gen rules (mkStdGen 105, ss) !! 4
> fooTest = tShow $ toPairs $ expand2 0 [] $ foo

> tFormat ts = map (\(a,b) -> (a, onset b, dur b)) ts
> tFormat2 ts = map (\(a,b) -> (a, fromRational(onset b)::Double, fromRational(dur b) :: Double)) ts
> tShow :: (Show a) => [(a,MP)] -> IO ()
> tShow = putStrLn . concat . intersperse "\n" . map show . tFormat2