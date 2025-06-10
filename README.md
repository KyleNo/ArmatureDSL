# ArmatureDSL

An shallowly embedded Domain-Specific Language for creating armatures.

## Requirements
* [cabal](https://www.haskell.org/cabal/)

## Writing Programs

### Preamble
ArmatureDSL is shallowly embedded, so programs are Haskell modules. Each program show start with:
```
module <ModuleName> where
import ArmatureDSL
```

### Basic objects

Armatures are constructed by combining `Bone`s and `Joint`s. Here is how to define them:

```
b :: Bone
b = bone <length>

j :: Joint
j = joint <rotation>
```

Lengths are float values. Rotations can be defined in a few ways, each of which corresponds to Euler angle representation.

```
straight
rotx <angle>
roty <angle>
rotz <angle>
xyz <angle> <angle> <angle>
```

`Bone`s can be attached to other `Bone`s via `Joint`s. The `+>` operator attaches a list of `Joint`-`Bone` connections to the end a bone. Connections are created with the `.->` operator.

```
b1 :: Bone
b1 = bone <length> +> [<joint> .-> <bone>, ...]
```

`Joint`s can be constrained with the `#>` operator.

```
j1 :: Joint
j1 = joint <rotation> #> [(freeze (X|Y|Z) <angle>
                          |range (X|Y|Z) <angle> <angle>
                          |ik <chain length>),
                          ...]
```

An armature is a defined by a root `Bone` and a location in space.
```
a :: Armature
a = arm (<x>, <y>, <z>) <bone>
```

### Operators and Combinators

Apply a rotation to a `Joint` with `@>`.

```
j2 :: Joint
j2 = j1 @> <rotation>
```

Apply a rotation to a rotation with `@@`.
```
r :: Rotation
r = <rotation> @@ <rotation>
```

Scale a `Bone` by a scalar with `.*`. Scale a `Bone` and all descendents with `.**`.

```
b1 :: Bone
b1 = b .* <scale factor>

b2 :: Bone
b2 = b .** <scale factor>
```

Combine `Bone`s and `Joint`s with `-+-` and `.+.`.
```
b2 :: Bone
b2 = b -+- b1

j2 :: Joint
j2 = j .+. j1
```

### Output
The armatures created with this DSL can be loaded into Blender by running the Python script that is generated.

```
main :: IO()
main = writeArm <armature> <filename>

-- or

main :: IO()
main = printArm <armature>
```

## Running Programs

```
cabal build
cabal repl

...

ghci> :l "examples\Hand.hs"
ghci> :main
```
