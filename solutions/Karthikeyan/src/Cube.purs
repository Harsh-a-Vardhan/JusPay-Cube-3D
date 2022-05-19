module Cube
  ( Action(..)
  , AngVelocity3D
  , Angle
  , Angle3D
  , Axis
  , Cube
  , Distance
  , Edge
  , Point2D
  , Point3D
  , Query(..)
  , Shape
  , State
  , accelerateBy
  , anglePerFrame
  , cubes
  , dampenAngVelocity
  , dampenPercent
  , decVel
  , frameRate
  , incAngVelocity
  , incVel
  , initCube
  , oneDegInRad
  , renderView
  , revDir
  , rotate
  , rotateShape
  , tenDegInRad
  , tick
  , viewBoxSize
  , viewCenter
  )
  where

import Data.Tuple
import Prelude

import Data.Array (index, mapWithIndex, take, tail,  (!!))
import Data.Array.NonEmpty (findIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Math (cos, sin)

-- Core Types
type Distance = Number

type Angle = Number

type Point2D =
  { x :: Distance
  , y :: Distance
  }

type Point3D =
  { x :: Distance
  , y :: Distance
  , z :: Distance
  }

type Edge = Tuple Int Int

type Shape =
  { vertices :: Array Point3D
  , edges :: Array Edge
  }

type Angle3D =
  { xa :: Angle
  , ya :: Angle
  , za :: Angle
  }

type AngVelocity3D = Angle3D -- velocity = angle/sec

type Cube =
  { shape :: Shape
  , angVel :: AngVelocity3D
  , forward :: Boolean
  , speed_factor :: Number
  }

data Axis = X | Y | Z

-- Model / State
type State = {cubes :: Array Cube}

-- Values

viewBoxSize :: Number
viewBoxSize = 600.0

viewCenter :: Point2D
viewCenter =
  { x: viewBoxSize / 2.0
  , y: viewBoxSize / 2.0
  }

frameRate :: Number
frameRate = 200.0

oneDegInRad :: Angle
oneDegInRad = 0.01745329255

tenDegInRad :: Angle
tenDegInRad = oneDegInRad * 10.0

accelerateBy :: Number
accelerateBy = oneDegInRad * 50.0

dampenPercent :: Number
dampenPercent = 1.0 - (0.9 / frameRate) -- 10% per second

initCube :: Cube
initCube =
  { shape:
  --point3D
      { vertices:
          [ { x:  100.0, y:  100.0, z:  100.0 }
          , { x: -100.0, y:  100.0, z:  100.0 }
          , { x:  100.0, y: -100.0, z:  100.0 }
          , { x: -100.0, y: -100.0, z:  100.0 }
          , { x:  100.0, y:  100.0, z: -100.0 }
          , { x: -100.0, y:  100.0, z: -100.0 }
          , { x:  100.0, y: -100.0, z: -100.0 }
          , { x: -100.0, y: -100.0, z: -100.0 }
          ]
  --Tuples
      , edges:
          [ Tuple 0 1
          , Tuple 0 2
          , Tuple 0 4
          , Tuple 1 5
          , Tuple 1 3
          , Tuple 2 3
          , Tuple 2 6
          , Tuple 4 5
          , Tuple 4 6
          , Tuple 3 7
          , Tuple 6 7
          , Tuple 5 7
          ]
      }
    --AngleVelocity
  , angVel:
      { xa: tenDegInRad
      , ya: tenDegInRad
      , za: tenDegInRad
      }
      --Direction
  , forward: true
  , speed_factor: 1.0
  }

data Query a = Tick a | Other a

-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis
  | Reversedirection Int
  | IncVelocity Int
  | DecVelocity Int
  | Add
  | Del


cubes :: forall query input output m. H.Component Query input output m
cubes =
    H.mkComponent
        { initialState: const initialState
        , render
        , eval: H.mkEval $ H.defaultEval 
              { 
                handleAction = handleAction 
              , handleQuery = handleQuery
              }
        }
    where
        initialState :: State
        initialState = {cubes:[initCube]}

        render :: forall m. State -> H.ComponentHTML Action () m
        render = renderView

        runFunction :: _ -> H.HalogenM State Action () output m Unit
        runFunction fn = do
          _ <- H.modify fn
          pure unit

        handleAction :: Action -> H.HalogenM State Action () output m Unit
        handleAction query = case query of
            DecAngVelocity axis -> H.modify_ \state -> state
            IncAngVelocity axis -> runFunction  (\c ->  ({cubes : incAngVelocity axis c.cubes}))
            Reversedirection i-> runFunction (revDirCubes i)
            IncVelocity i -> runFunction (incVelCubes i)
            DecVelocity i -> runFunction (decVelCubes i)
            Add -> runFunction (addCube)
            Del -> runFunction (delCube)

        handleQuery :: forall m a message. Query a -> H.HalogenM State Action () message m (Maybe a)
        handleQuery = case _ of
          Tick a -> do
            _ <- H.modify (\c -> tickCubes c)
            pure (Just a)
          Other a -> 
            pure (Just a)

revDirCubes :: Int ->  State -> State 
revDirCubes i state = state {cubes = mapWithIndex ( \index cube -> if index == i then revDir cube else cube) state.cubes}

incVelCubes :: Int -> State -> State 
incVelCubes i state = state {cubes = mapWithIndex (\index cube -> if index == i then incVel cube else cube ) state.cubes}

decVelCubes :: Int -> State -> State 
decVelCubes i state = state {cubes = mapWithIndex (\index cube -> if index == i then decVel cube else cube ) state.cubes}

revDir :: Cube -> Cube
revDir c = do
  c { forward = not c.forward }

tickCubes :: State -> State
tickCubes state = state {cubes = map tick state.cubes}
addCube :: State -> State 
addCube state = let 
  cubes = state.cubes
  in 
  state {cubes = cubes <> [initCube]}

delCube state = let 
  cubes = tail state.cubes
  in 
  case cubes of 
    Just cube -> state {cubes = cube}
    _ -> state {cubes = []}


incVel :: Cube -> Cube
incVel c = do
  c { speed_factor = c.speed_factor * 2.0 }

decVel :: Cube -> Cube
decVel c = do
  c { speed_factor = c.speed_factor / 2.0 }

incAngVelocity :: Axis -> Array Cube -> Array Cube
incAngVelocity axis cubes = map (incAngVelocityCube axis) cubes
incAngVelocityCube :: Axis -> Cube -> Cube
incAngVelocityCube axis c = do 
  let {xa, ya, za} = c.angVel
  if c.forward == true then
    case axis of
      X -> c { angVel { xa = xa + accelerateBy * c.speed_factor } }
      Y -> c { angVel { ya = ya + accelerateBy * c.speed_factor } }
      Z -> c { angVel { za = za + accelerateBy * c.speed_factor } }
  else case axis of
      X -> c { angVel { xa = xa - accelerateBy * c.speed_factor } }
      Y -> c { angVel { ya = ya - accelerateBy * c.speed_factor } }
      Z -> c { angVel { za = za - accelerateBy * c.speed_factor } }

-- decAngVelocity :: Axis -> Cube -> Cube
-- decAngVelocity axis c = do 
--   let {xa, ya, za} = c.angVel
--   case axis of
--     X -> c { angVel { xa = xa - accelerateBy } }
--     Y -> c { angVel { ya = ya - accelerateBy } }
--     Z -> c { angVel { za = za - accelerateBy } }

tick :: Cube -> Cube
tick c =  do
  let angVel = c.angVel
      {vertices, edges} = c.shape
      newShape =
        { edges : edges
        , vertices: rotateShape vertices (anglePerFrame angVel)
        }
      newCube = c
        { angVel = dampenAngVelocity angVel
        , shape = newShape
        }
  newCube


rotateShape :: Array Point3D -> AngVelocity3D -> Array Point3D
rotateShape vertices ang =
  map (rotate ang) vertices

rotate :: AngVelocity3D -> Point3D -> Point3D
rotate { xa, ya, za } = rotateX xa >>> rotateY ya >>> rotateZ za
  where
    rotateX ang {x,y,z} = let Tuple ny nz = rotateInPlane y z ang in { x, y:ny, z:nz }
    rotateY ang {x,y,z} = let Tuple nx nz = rotateInPlane x z ang in { x:nx, y, z:nz }
    rotateZ ang {x,y,z} = let Tuple nx ny = rotateInPlane x y ang in { x:nx, y:ny, z }

    rotateInPlane :: Number -> Number -> Number -> Tuple Number Number
    rotateInPlane axis1 axis2 ang =
      Tuple (axis1 * cos(ang) - axis2 * sin(ang)) (axis2 * cos(ang) + axis1 * sin(ang))

anglePerFrame :: AngVelocity3D -> Angle3D
anglePerFrame {xa, ya, za} =
  { xa: xa / frameRate
  , ya: ya / frameRate
  , za: za / frameRate
  }

dampenAngVelocity :: AngVelocity3D -> AngVelocity3D
dampenAngVelocity {xa, ya, za} =
    { xa: dampen xa
    , ya: dampen ya
    , za: dampen za
    }
  where
    dampen :: Number -> Number
    dampen ang = ang * dampenPercent -- Basics.max 0 (ang-drpf)


---------------------------------------------------------------------------------------



renderView :: forall m. State -> H.ComponentHTML Action () m
renderView state = 
        HH.div [] $
        [ renderButton "rotX++" (IncAngVelocity X)
        , renderButton "rotY++" (IncAngVelocity Y)
        , renderButton "rotZ++" (IncAngVelocity Z)
        , renderButton "AddCube" (Add)
        , renderButton "DeleteCube" (Del)
        -- , renderButton "Reverse" (Reversedirection)
        -- , renderButton "Inc Vel" (IncVelocity)
        -- , renderButton "Dec Vel" (DecVelocity)
        ]
        <>
        (mapWithIndex drawCubes state.cubes)
    where
        drawCubes i cube = let 
          {vertices,edges} = cube.shape
          vert2Ds = map project vertices
          in 
           HH.div [] $
           [  renderButton "Reverse" (Reversedirection i)
            , renderButton "Inc Vel" (IncVelocity i)
            , renderButton "Dec Vel" (DecVelocity i)
           ] <>
           [ SE.svg
            [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
            [ SE.g []
            (drawCube edges vert2Ds)
            ]
           ]

        renderButton label query =
            HH.button
            [ HP.title label
            , HE.onClick (\_ -> query)
            ]
            [ HH.text label ]

        -- parallel projection
        project :: Point3D -> Point2D
        project p =
            { x: p.x + viewCenter.x
            , y: p.y + viewCenter.y
            }

        drawCube :: forall m. Array Edge -> Array Point2D -> Array (H.ComponentHTML Action () m)
        drawCube edges vert2Ds =
            drawEdges edges vert2Ds <> drawVertices vert2Ds

        drawEdges :: forall m. Array Edge -> Array Point2D -> Array (H.ComponentHTML Action () m)
        drawEdges edges verts = let
            connectedVerts = map (\(Tuple v1 v2) -> Tuple (verts !! v1) (verts !! v2)) edges
            in
            map (\(Tuple v1 v2) -> drawLine (getPoint v1) (getPoint v2)) connectedVerts

        getPoint :: Maybe Point2D -> Point2D
        getPoint maybePoint = let
            default = { x: 100.0, y: 100.0 }
            in
            fromMaybe default maybePoint

        drawVertices :: forall m. Array Point2D -> Array (H.ComponentHTML Action () m)
        drawVertices vert2Ds =
            mapWithIndex drawVertex vert2Ds


        drawLine :: forall m. Point2D -> Point2D -> H.ComponentHTML Action () m
        drawLine a b =
            SE.line 
            [
              SA.x1 a.x
            , SA.x2 b.x
            , SA.y1 a.y
            , SA.y2 b.y
            , SA.stroke $ Just (SA.RGB 50 50 50)
            ]

        drawVertex :: forall m. Int -> Point2D -> H.ComponentHTML Action () m
        drawVertex idx {x, y} = SE.g []
            [ SE.text
                [ SA.x $ x + 5.0
                , SA.y $ y - 5.0
                , SA.fill $ Just (SA.RGB 150 150 150)
                ]
                [ HH.text $ show idx ]
            , SE.circle
                [ SA.r 3.0
                , SA.cx x
                , SA.cy y
                , SA.fill $ Just (SA.RGB 100 100 100)
                ]
            ]