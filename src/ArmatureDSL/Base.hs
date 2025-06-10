module ArmatureDSL.Base where

import Linear hiding (angle, rotate)
import Prelude hiding (head, tail, pi)
import qualified Prelude (pi)

-- Types

type Length = Double
type Pos    = V3 Double
type Mat3   = M33 Double
-- in degrees
type Angle  = Double

pi :: Double
pi = Prelude.pi

toRad :: Angle -> Double
toRad deg = deg * pi / 180

fromRad :: Double -> Angle
fromRad rad = rad * 180 / pi

data Axis = X | Y | Z deriving (Show, Eq)

-- Rotation and Constraints

data Rotation = Rotation
    { roll  :: Angle
    , pitch :: Angle
    , yaw   :: Angle
    } deriving Show

data RotationLimit
    = Free
    | Frozen Angle
    | Range Angle Angle
    deriving Show

data Constraint
    = RotCon Axis RotationLimit
    | ChainLength Int
    deriving Show

type Constraints = [Constraint]

-- AST

data Armature = Armature
    { position :: Pos
    , root     :: Bone
    }

data Bone = Extremity Length
          | WithChildren Length Connections
          deriving Show

data Connection = Connect Joint Bone
                deriving Show

type Connections = [Connection]

data Joint = Joint { rotation :: Rotation
                   , constraints :: Constraints
                   } deriving Show