module ArmatureDSL.Eval where

import ArmatureDSL.Base
import ArmatureDSL.Util

import Linear hiding (angle, rotate)
import Prelude hiding (head, tail, pi)
import System.Directory
import System.FilePath

-- evaluation

-- count the number of bones in an armature

countBone :: Bone -> Int
countBone (Extremity _) = 1
countBone (WithChildren _ cs) = 1 + sum (map (countBone . extractBone) cs)

count :: Armature -> Int
count (Armature _ b) = countBone b

-- Create python file to generate armature

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

-- print V3 as a tuple, which python will accept
showV3 :: Show a => V3 a -> String
showV3 (V3 x y z) = show (x, y, z)

-- Create python file with 
genBlender :: Armature -> String
genBlender (Armature origin rootBone) =
    let ab = genArmatureBase origin
        bs = genRootBone rootBone origin
        EP e p = bs
        edit = (ab ++ e)
        pose = genPose p
    in genPreamble ++ edit ++ pose


genPreamble :: String
genPreamble = "import bpy\n\
              \from mathutils import Euler\n\n\
              \for block in bpy.data.armatures:\n\
              \    bpy.data.armatures.remove(block)\n\n\
              \if bpy.ops.object.mode_set.poll():\n\
              \    bpy.ops.object.mode_set(mode='OBJECT')\n\
              \bpy.ops.object.select_all(action='DESELECT')\n\n"

data EditPose = EP
    { editCode :: String
    , poseCode :: String
    }

instance Semigroup EditPose where
    EP e1 p1 <> EP e2 p2 = EP (e1 ++ e2) (p1 ++ p2)

instance Monoid EditPose where
    mempty = EP "" ""

genArmatureBase :: Pos -> String
genArmatureBase origin =
    "bpy.ops.object.armature_add(enter_editmode=True, location=" ++ showV3 origin ++ ")\n\
    \armature = bpy.context.object\n\
    \armature.name = " ++ quoted "arm" ++ "\n\
    \armature_data = armature.data\n\
    \armature_data.name = " ++ quoted "arm" ++ "\n\n\
    \bpy.ops.object.mode_set(mode=\"EDIT\")\n\
    \edit_bones = armature_data.edit_bones\n\n"

genPose :: String -> String
genPose p =
    "bpy.ops.object.mode_set(mode=\"POSE\")\n\
    \pose_bones = armature.pose.bones\n" ++ p

genRootBoneHelper :: Pos -> Pos -> EditPose
genRootBoneHelper head tail = EP
    ("root_bone = edit_bones[0]\n\
    \root_bone.name = " ++ quoted "root_bone" ++ "\n\
    \root_bone.head = " ++ showV3 head ++ "\n\
    \root_bone.tail = " ++ showV3 tail ++ "\n\n")
    ""

genRootBone :: Bone -> Pos -> EditPose
genRootBone b pos =
    let l = getLength b
        tail = pos ^+^ V3 0 0 l
        rb = genRootBoneHelper pos tail
        cs = case b of
            WithChildren _ cnxs ->
                genBones cnxs tail "bone_" 0 "root_bone"
            _ -> mempty
    in rb <> cs

genBones :: Connections -> Pos -> String -> Int -> String -> EditPose
genBones [] _ _ _ _ = mempty
genBones (Connect j b : cnxs) head prefix i parent =
    let var = prefix ++ show i
        l = getLength b
        tail = head ^+^ V3 0 0 l
        be = var ++ " = edit_bones.new(" ++ quoted var ++ ")\n" ++
             var ++ ".head = " ++ showV3 head ++ "\n" ++
             var ++ ".tail = " ++ showV3 tail ++ "\n" ++
             var ++ ".parent = " ++ parent ++ "\n" ++
             var ++ ".use_connect = True\n\n"
        Joint rot cons = j
        Rotation r p y = rot
        -- bp = "pb = pose_bones[" ++ var ++ ".name]\n\
        -- can't rely on edit_bones vars being accessible after context change
        bp = "pb = pose_bones[" ++ quoted var ++ "]\n\
             \pb.matrix_basis = Euler(" ++ showV3 (fmap toRad (V3 r p y)) ++ ", \"XYZ\").to_matrix().to_4x4()\n\n" ++
             genConstraints cons
        ep = EP be bp
        cep = case b of
            WithChildren _ ccnxs -> genBones ccnxs tail (var ++ "_") 0 var
            _ -> mempty
        rest = genBones cnxs head prefix (i+1) parent
    in ep <> cep <> rest

genConstraints :: Constraints -> String
genConstraints cons =
    let scons = simplifyConstraints cons
        rotCons = genRotConstraints scons
        rotOut = case rotCons of
            "" -> ""
            _ -> "con = pb.constraints.new(\"LIMIT_ROTATION\")\n" ++
                 rotCons ++
                 "con.owner_space = \"LOCAL\"\n"
        chainOut = genChainConstraint scons
    in rotOut ++ chainOut

genChainConstraint :: Constraints -> String
genChainConstraint (ChainLength n:_) =
    "con = pb.constraints.new(\"IK\")\n\
    \con.chain_count = " ++ show n ++ "\n"
genChainConstraint [] = ""
genChainConstraint (_:cons) = genChainConstraint cons

lowerAxis :: Axis -> String
lowerAxis X = "x"
lowerAxis Y = "y"
lowerAxis Z = "z"

genRotConstraints :: Constraints -> String
genRotConstraints = concatMap genRotConstraint

genRotConstraint :: Constraint -> String
genRotConstraint (RotCon axis (Frozen angle)) =
    let a = lowerAxis axis
        ang = toRad angle
    in "con.use_limit_" ++ a ++ " = True\n\
       \con.min_" ++ a ++ " = " ++ show ang ++ "\n\
       \con.max_" ++ a ++ " = " ++ show ang ++ "\n"
genRotConstraint (RotCon axis (Range angle1 angle2)) =
    let a = lowerAxis axis
        ang1 = toRad angle1
        ang2 = toRad angle2
    in "con.use_limit_" ++ a ++ " = True\n\
       \con.min_" ++ a ++ " = " ++ show ang1 ++ "\n\
       \con.max_" ++ a ++ " = " ++ show ang2 ++ "\n"
genRotConstraint _ = ""

-- write generated code to file
writeArm :: Armature -> String -> IO()
writeArm armature filename = do
    createDirectoryIfMissing True (takeDirectory filename)
    writeFile filename (genBlender armature)

-- print generated code to console
printArm :: Armature -> IO()
printArm armature = do
    putStrLn (genBlender armature)
