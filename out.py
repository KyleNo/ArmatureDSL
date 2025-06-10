import bpy
from mathutils import Euler

for block in bpy.data.armatures:
    bpy.data.armatures.remove(block)

if bpy.ops.object.mode_set.poll():
    bpy.ops.object.mode_set(mode='OBJECT')
bpy.ops.object.select_all(action='DESELECT')

bpy.ops.object.armature_add(enter_editmode=True, location=(0.0,0.0,0.0))
armature = bpy.context.object
armature.name = "arm"
armature_data = armature.data
armature_data.name = "arm"

bpy.ops.object.mode_set(mode="EDIT")
edit_bones = armature_data.edit_bones

root_bone = edit_bones[0]
root_bone.name = "root_bone"
root_bone.head = (0.0,0.0,0.0)
root_bone.tail = (0.0,0.0,4.0)

bone_0 = edit_bones.new("bone_0")
bone_0.head = (0.0,0.0,4.0)
bone_0.tail = (0.0,0.0,5.2)
bone_0.parent = root_bone
bone_0.use_connect = True

bone_0_0 = edit_bones.new("bone_0_0")
bone_0_0.head = (0.0,0.0,5.2)
bone_0_0.tail = (0.0,0.0,6.300000000000001)
bone_0_0.parent = bone_0
bone_0_0.use_connect = True

bone_0_0_0 = edit_bones.new("bone_0_0_0")
bone_0_0_0.head = (0.0,0.0,6.300000000000001)
bone_0_0_0.tail = (0.0,0.0,7.300000000000001)
bone_0_0_0.parent = bone_0_0
bone_0_0_0.use_connect = True

bone_1 = edit_bones.new("bone_1")
bone_1.head = (0.0,0.0,4.0)
bone_1.tail = (0.0,0.0,5.4399999999999995)
bone_1.parent = root_bone
bone_1.use_connect = True

bone_1_0 = edit_bones.new("bone_1_0")
bone_1_0.head = (0.0,0.0,5.4399999999999995)
bone_1_0.tail = (0.0,0.0,6.76)
bone_1_0.parent = bone_1
bone_1_0.use_connect = True

bone_1_0_0 = edit_bones.new("bone_1_0_0")
bone_1_0_0.head = (0.0,0.0,6.76)
bone_1_0_0.tail = (0.0,0.0,7.96)
bone_1_0_0.parent = bone_1_0
bone_1_0_0.use_connect = True

bone_2 = edit_bones.new("bone_2")
bone_2.head = (0.0,0.0,4.0)
bone_2.tail = (0.0,0.0,5.2)
bone_2.parent = root_bone
bone_2.use_connect = True

bone_2_0 = edit_bones.new("bone_2_0")
bone_2_0.head = (0.0,0.0,5.2)
bone_2_0.tail = (0.0,0.0,6.300000000000001)
bone_2_0.parent = bone_2
bone_2_0.use_connect = True

bone_2_0_0 = edit_bones.new("bone_2_0_0")
bone_2_0_0.head = (0.0,0.0,6.300000000000001)
bone_2_0_0.tail = (0.0,0.0,7.300000000000001)
bone_2_0_0.parent = bone_2_0
bone_2_0_0.use_connect = True

bpy.ops.object.mode_set(mode="POSE")
pose_bones = armature.pose.bones
pb = pose_bones[bone_0.name]
pb.matrix_basis = Euler((-0.17453292519943295,0.0,0.0), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = -0.2617993877991494
con.max_x = -8.726646259971647e-2
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_0_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_0_0_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_1.name]
pb.matrix_basis = Euler((0.0,0.0,0.0), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = -8.726646259971647e-2
con.max_x = 8.726646259971647e-2
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_1_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_1_0_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_2.name]
pb.matrix_basis = Euler((0.17453292519943295,0.0,0.0), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 8.726646259971647e-2
con.max_x = 0.2617993877991494
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_2_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
pb = pose_bones[bone_2_0_0.name]
pb.matrix_basis = Euler((0.0,0.0,0.7853981633974483), "ZYX").to_matrix().to_4x4()

con = pb.constraints.new("LIMIT_ROTATION")
con.use_limit_x = True
con.min_x = 0.0
con.max_x = 0.0
con.use_limit_y = True
con.min_y = 0.0
con.max_y = 0.0
con.use_limit_z = True
con.min_z = 0.0
con.max_z = 1.5707963267948966
con.owner_space = "LOCAL"
