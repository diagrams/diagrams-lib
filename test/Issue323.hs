{- From bug report by Mike Zuser (Issue #323):

    segmentSegment can fail to terminate on very specific and seemingly innocuous inputs
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Diagrams.Prelude
import Diagrams.TwoD.Segment

d = 72       -- ~ 71.9 to 72.1
r = d/64     -- ~ 64 to 64.0000001
e = 6.969e-8 -- ~ <= 6.969e-8

path :: Path V2 Double
path = circle r # translateY d

trails :: [Located (Trail V2 Double)]
trails = head $ explodePath path

(s0:s1:_) = map (head . fixTrail) trails

bad = segmentSegment e s0 s1

-- Does not terminate or produce output.
main = print bad
