module Tree where

import ArmatureDSL

leaf = unit

lj = joint (zrot (-20) @@ xrot 45)
rj = joint (zrot 20 @@ xrot (-45))

n1 = bone 1 +> [lj .-> leaf, rj .-> leaf]

n2 = bone 2 +> [lj .-> n1, rj .-> n1]

n3 = bone 4 +> [lj .-> n2, rj .-> n2]

n4 = bone 8 +> [lj .-> n3, rj .-> n3]

tree = arm (0, 0, 0) n4

main :: IO ()
main = writeArm tree "output\\tree.py"