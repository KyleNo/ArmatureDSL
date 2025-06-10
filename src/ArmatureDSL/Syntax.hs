module ArmatureDSL.Syntax where

import ArmatureDSL.Base
import ArmatureDSL.Util

import Linear hiding (angle, rotate)

-- Smart constructors

arm :: (Double, Double, Double) -> Bone -> Armature
arm (x, y, z) = Armature (V3 x y z)

bone :: Length -> Bone
bone = Extremity

joint :: Rotation -> Joint
joint rot = Joint rot []

unit :: Bone
unit = bone 1

straight :: Rotation
straight = Rotation 0 0 0

xyz :: Angle -> Angle -> Angle -> Rotation
xyz = Rotation

freeze :: Axis -> Angle -> Constraint
freeze a angle = RotCon a (Frozen angle)

range :: Axis -> Angle -> Angle -> Constraint
range a ang1 ang2 = RotCon a (Range ang1 ang2)

ik :: Int -> Constraint
ik = ChainLength

(.->) :: Joint -> Bone -> Connection
(.->) = Connect
infix 6 .->

connect :: Joint -> Bone -> Connection
connect = Connect

(-+-) :: Bone -> Bone -> Bone
(-+-) = combineBones
infixr 5 -+-

(+..) :: Bone -> Bone -> Bone
base +.. sub = attachToLeaves base sub
infixr 4 +..

rotate :: Joint -> Rotation -> Joint
rotate j r = j { rotation = applyConstraints (constraints j) (combineRotation (rotation j) r) }

(@>) :: Joint -> Rotation -> Joint
(@>) = rotate
infix 8 @>

constrain :: Joint -> Constraints -> Joint
constrain j cs = j { constraints = combineConstraints cs  (constraints j), rotation = applyConstraints (cs ++ constraints j) (rotation j) }

(#>) :: Joint -> Constraints -> Joint
(#>) = constrain
infix 7 #>

xrot, yrot, zrot :: Angle -> Rotation
xrot r = Rotation r 0 0
yrot p = Rotation 0 p 0
zrot y = Rotation 0 0 y

combineRotation :: Rotation -> Rotation -> Rotation
combineRotation r1 r2 =
    let m1 = rotToMatrix r1
        m2 = rotToMatrix r2
        m = m2 !*! m1
    in matrixToRot m

(@@) :: Rotation -> Rotation -> Rotation
(@@) = combineRotation
infix 4 @@

combineJoints :: Joint -> Joint -> Joint
combineJoints j1 (Joint rot2 cons2) = (j1 #> cons2) @> rot2

(.+.) :: Joint -> Joint -> Joint
(.+.) = combineJoints
infix 7 .+.



-- Bone scaling

-- scale a bone by a certain factor
scale :: Bone -> Length -> Bone
scale (Extremity l) factor = Extremity (l * factor)
scale (WithChildren l cs) factor = WithChildren (l * factor) cs

-- recursively scale all bones by a factor
scaleR :: Bone -> Length -> Bone
scaleR (Extremity l) factor = Extremity (l * factor)
scaleR (WithChildren l cs) factor =
    WithChildren (l * factor) (map scaleConn cs)
        where
            scaleConn (Connect j b) = Connect j (b `scaleR` factor)

(.*) :: Bone -> Length -> Bone
(.*) = scale
infix 7 .*

(.**) :: Bone -> Length -> Bone
(.**) = scaleR
infix 7 .**

-- Bone attachment and merging

withChildren :: Bone -> Connections -> Bone
withChildren b [] = b
withChildren (Extremity l) cnxs = WithChildren l cnxs
withChildren (WithChildren l cnxs1) cnxs2 = WithChildren l (cnxs1 ++ cnxs2)

(+>) :: Bone -> Connections -> Bone
(+>) = withChildren
infixr 6 +>

attachToLeaves :: Bone -> Bone -> Bone
attachToLeaves base sub = descendAttach base
    where
        descendAttach (Extremity l) =
            WithChildren l [Connect (joint straight) sub]
        descendAttach (WithChildren l cs) =
            WithChildren l (map updateChild cs)
        updateChild (Connect j child) = Connect j (descendAttach child)

combineBones :: Bone -> Bone -> Bone
combineBones b1 (Extremity _) = b1
combineBones (WithChildren l1 cs1) (WithChildren _ cs2) =
    WithChildren l1 (cs1 ++ cs2)
combineBones (Extremity l1) (WithChildren _ cs2) =
    WithChildren l1 cs2

mergeAllBones :: Bone -> [Bone] -> Bone
mergeAllBones = foldl combineBones