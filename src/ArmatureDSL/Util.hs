module ArmatureDSL.Util where

import ArmatureDSL.Base

import Linear hiding (angle, rotate)
import Data.Fixed (mod')

-- Helper functions

addPos :: Pos -> Pos -> Pos
addPos = (^+^)

-- normalize angle between 0 and 360
normAngle :: Angle -> Angle
normAngle x = let x' = mod' x 360 in if x' < 0 then x' + 360 else x'

-- check if lo <= x <= hi (mod 360)
inRange :: Angle -> Angle -> Angle -> Bool
inRange lo hi x =
    let x' = normAngle x
        lo' = normAngle lo
        hi' = normAngle hi
    in if lo' <= hi'
       -- lo x hi 
       then lo' <= x' && x' <= hi'
       -- hi lo x
       -- x hi lo
       else x' >= lo' || x' <= hi'

-- smallest distance between two angles
angleDiff :: Angle -> Angle -> Angle
angleDiff a b =
    let d = abs (normAngle a - normAngle b)
    in min d (360 - d)

clamp :: RotationLimit -> Angle -> Angle
clamp Free a = a
clamp (Frozen f) _ = f
clamp (Range lo hi) a
    | inRange lo hi a = a
    -- clamp to the closest extreme
    | otherwise       = if angleDiff a lo < angleDiff a hi then lo else hi

-- combine two RotationLimits. If any constraints are incompatible throw an error
combineLimits :: RotationLimit -> RotationLimit -> RotationLimit
-- if either Free, use other constraint
combineLimits Free r = r
combineLimits r Free = r
combineLimits (Frozen f1) (Frozen f2)
    -- these need to be same within some tolerance
    | abs (f1 - f2) <= 1e-6 = Frozen f1
    -- | otherwise = Range (min f1 f2) (max f1 f2)
    -- | otherwise = error (format (string % float % string % float) "Incompatible frozen constraints: " f1 " and " f2)
    | otherwise = error "Incompatible frozen constraints"
combineLimits (Frozen f) (Range lo hi)
    | inRange lo hi f = Frozen f
    -- | otherwise       = Range lo hi
    | otherwise       = error "Frozen constraint not inside range"
combineLimits (Range lo hi) (Frozen f)
    | inRange lo hi f = Frozen f
    -- | otherwise       = Range lo hi
    | otherwise       = error "Frozen constraint not inside range"
-- intersect two ranges.
combineLimits (Range lo1 hi1) (Range lo2 hi2) =
    case (inRange lo1 hi1 lo2, inRange lo1 hi1 hi2) of
        -- lo1 lo2 hi2 hi1
        (True, True)   -> Range lo2 hi2
        -- lo1 lo2 hi1 hi2
        (True, False)  -> Range lo2 hi1
        -- lo2 lo1 hi2 hi1
        (False, True)  -> Range lo1 hi2
        (False, False) ->
            -- lo2 lo1 hi1 hi2
            if inRange lo2 hi2 lo1 && inRange lo2 hi2 hi1 then Range lo1 hi1
            -- lo1 hi1 lo2 hi2
            else error "Non overlapping ranges"

combineChains :: Int -> Int -> Int
combineChains = max

simplifyConstraints :: Constraints -> Constraints
simplifyConstraints cons =
    let axisLimits axis = [lim | RotCon a lim <- cons, a == axis]
        combined axis = foldr combineLimits Free (axisLimits axis)
        chainLengths = [n | ChainLength n <- cons]
        combinedChains = if null chainLengths then 0 else maximum chainLengths
        allConstraints = [RotCon X (combined X), RotCon Y (combined Y), RotCon Z (combined Z), ChainLength combinedChains]

        keep cons_ = case cons_ of
            RotCon _ Free -> False
            ChainLength 0 -> False
            _             -> True
    in filter keep allConstraints

combineConstraints :: Constraints -> Constraints -> Constraints
combineConstraints cs1 cs2 = simplifyConstraints (cs1 ++ cs2)

applyConstraints :: Constraints -> Rotation -> Rotation
applyConstraints cs (Rotation r p y) =
    let
        axisLimits axis = [lim | RotCon a lim <- cs, a == axis]

        clamped axis angle =
            let limits = axisLimits axis
                combined = foldr combineLimits Free limits
            in clamp combined angle
    in Rotation (clamped X r) (clamped Y p) (clamped Z y)

extractBone :: Connection -> Bone
extractBone (Connect _ b) = b

getLength :: Bone -> Length
getLength (Extremity l)      = l
getLength (WithChildren l _) = l

-- Rotation matrix math

yawMatrix :: Angle -> M33 Double
yawMatrix y =
    let y' = toRad y in V3
        (V3 (cos y') (-sin y') 0)
        (V3 (sin y') (cos y')  0)
        (V3 0        0         1)

pitchMatrix :: Angle -> M33 Double
pitchMatrix p =
    let p' = toRad p in V3
        (V3 (cos p')  0 (sin p'))
        (V3 0    1        0)
        (V3 (-sin p') 0 (cos p'))

rollMatrix :: Angle -> M33 Double
rollMatrix z =
    let z' = toRad z in V3
        (V3 1        0         0)
        (V3 0 (cos z') (-sin z'))
        (V3 0 (sin z') (cos z') )

rotToMatrix :: Rotation -> M33 Double
rotToMatrix (Rotation r p y) = yawMatrix y !*! pitchMatrix p !*! rollMatrix r
-- rotToMatrix (Rotation r p y) = rollMatrix r !*! pitchMatrix p !*! yawMatrix y


matrixToRot :: M33 Double -> Rotation
matrixToRot m =
    let V3 (V3 m11 _   _  )
           (V3 m21 m22 m23)
           (V3 m31 m32 m33) = m

        sy = -m31
        cy = sqrt (m11 * m11 + m21 * m21)

        singular = cy < 1e-6

        (rx, ry, rz)
            | not singular =
                ( atan2 m32 m33
                , asin sy
                , atan2 m21 m11
                )
            | otherwise =
                ( atan2 (-m23) m22
                , asin sy
                , 0
                )
    in Rotation (fromRad rx) (fromRad ry) (fromRad rz)