{- From bug report by Felipe Lessa (Issue #57):

Hello!

I'm sorry that I didn't do my homework to investigate the "why", but I've included a very simple test case.  You may compile and run the test as follows:

$ ghc -O --make -fforce-recomp -rtsopts bug.hs
$ ./bug -o t.png --selection=??? +RTS -s

where ??? may be "hcat", "foldl1", "foldr1" or "foldTree".  The program just concatenates 1,000 (rect 1 1)s.  On my computer, I get the following:

hcat: 23s, 130 MiB
foldr1: 30s, 69 MiB
foldl1: 27s, 62 MiB
foldTree: 1s, 7 MiB

Concatenating one thousand unit squares shouldn't take more than 20 seconds =).  This is a showstopper for me, so I've reimplemented hcat (see hcatB at the end of [1])

-}

-- from diagrams-lib
import Diagrams.Prelude

-- from diagrams-cairo
import Diagrams.Backend.Cairo.CmdLine (Cairo, multiMain)

main :: IO ()
main = multiMain [ ("hcat",     hcat (dias n))
                 , ("foldr1",   foldr1 (|||) (dias n))
                 , ("foldl1",   foldl1 (|||) (dias n))
                 , ("foldTree", foldTree (|||) (dias n))
                 ]
    where
      n = 1000

dias :: Int -> [Diagram Cairo R2]
dias n = replicate n (rect 1 1)

foldTree :: (a -> a -> a) -> [a] -> a
foldTree f = go
    where
      go [x] = x
      go []  = error "foldTree: empty input"
      go xs  = go (twoByTwo xs)

      twoByTwo (x1:x2:xs) = f x1 x2 : twoByTwo xs
      twoByTwo xs         = xs
