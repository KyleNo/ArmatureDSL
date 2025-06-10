module Hand where

import ArmatureDSL


fingertip :: Bone
fingertip = unit

jc :: Constraints
jc = [freeze X 0, freeze Y 0, range Z 0 90]

j :: Joint
j = joint (zrot 45) #> jc

middlesegment :: Bone
middlesegment = bone 1.1 +> [j .-> fingertip]

finger :: Bone
finger = bone 1.2 +> [j .-> middlesegment]

j2 :: Joint
j2 = joint straight #> [freeze Y 0, range Z 0 90]

handBase :: Bone
handBase = bone 4 +> [j2 @> xrot (-10) #> [range X (-15) (-5)] .-> finger,
                      j2               #> [range X (-5) 5]     .-> finger .** 1.2,
                      j2 @> xrot 10    #> [range X 5 15]       .-> finger]
hand :: Armature
hand = arm (0, 0, 0) handBase

main :: IO ()
main = writeArm hand "output\\hand.py"