module Cube
  ( Action(..)
  , AngVelocity3D
  , Angle
  , Angle3D
  , Axis(..)
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
  , frameRate
  , incAngVelocity
  , increaseSpeed
  , initCube
  , oneDegInRad
  , renderView
  , reverseCube
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
import Data.Array (dropEnd, length, mapWithIndex, snoc, (!!))
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
  , speedControl :: Number
  , id :: Int
  }

data Axis = X | Y | Z

-- Model / State
type State = Array Cube

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
  , angVel:
      { xa: tenDegInRad
      , ya: tenDegInRad
      , za: tenDegInRad
      }
  , forward: true
  , speedControl: 1.0
  , id: 0
  }

data Query a = Tick a | Other a

-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis 
  | ReverseCube Int
  | IncreaseSpeed Int
  | DecreaseSpeed Int
  | AddCube
  | DeleteCube
  

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
        initialState = [initCube]

        render :: forall m. State -> H.ComponentHTML Action () m
        render state = HH.div[][HH.ul[]$map renderView state]

        runFunction :: _ -> H.HalogenM State Action () output m Unit
        runFunction fn = do
          _ <- H.modify fn
          pure unit

        handleAction :: Action -> H.HalogenM State Action () output m Unit
        handleAction query = case query of
            DecAngVelocity axis -> H.modify_ \state -> state
            IncAngVelocity axis -> runFunction  (\c -> map (incAngVelocity axis) c)
            ReverseCube id -> runFunction  (\c -> map (reverseCube id) c)
            IncreaseSpeed id -> runFunction  (\c -> map (increaseSpeed id) c)
            DecreaseSpeed id -> runFunction  (\c -> map (decreaseSpeed id) c)
            AddCube -> runFunction (\c -> snoc c initCube {id = (length c) } )
            DeleteCube -> runFunction (\c -> if length c > 1 then dropEnd 1 c else c)

        handleQuery :: forall m a message. Query a -> H.HalogenM State Action () message m (Maybe a)
        handleQuery = case _ of
          Tick a -> do
            _ <- H.modify (\c -> map tick c)
            pure (Just a)
          Other a -> 
            pure (Just a)

     
incAngVelocity :: Axis -> Cube -> Cube
incAngVelocity axis c = do 
  let {xa, ya, za} = c.angVel
  case axis of
    X -> c { angVel { xa = if c.forward then (xa + accelerateBy * c.speedControl) else (xa - accelerateBy * c.speedControl) } }
    Y -> c { angVel { ya = if c.forward then (ya + accelerateBy * c.speedControl) else (ya - accelerateBy * c.speedControl) } }
    Z -> c { angVel { za = if c.forward then (za + accelerateBy * c.speedControl) else (za - accelerateBy * c.speedControl) } }

reverseCube :: Int -> Cube -> Cube
reverseCube id c = if id==c.id then c{ forward = not c.forward } else c

increaseSpeed :: Int -> Cube -> Cube
increaseSpeed id c = if id==c.id then c { speedControl = c.speedControl * 2.0 } else c

decreaseSpeed :: Int -> Cube -> Cube
decreaseSpeed id c = if id==c.id then c { speedControl = c.speedControl / 2.0 } else c


{- addCube :: Cube -> Cube
addCube  CubeArray c
-}

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

renderView :: forall m. Cube -> H.ComponentHTML Action () m
renderView state = let
        {vertices, edges} = state.shape
        vert2Ds = map project vertices
    in
        HH.div [] $
        [ renderButton "rotX++" (IncAngVelocity X)
        , renderButton "rotY++" (IncAngVelocity Y)
        , renderButton "rotZ++" (IncAngVelocity Z)
        , renderButton "reverse" (ReverseCube state.id) 
        , renderButton "Vel++" (IncreaseSpeed state.id)
        , renderButton "Vel--" (DecreaseSpeed state.id)
        , renderButton "AddCube" (AddCube)
        , renderButton "DeleteCube" (DeleteCube)
        , HH.text("  Id: " <> show state.id <> " Direction: " <> show state.forward <> " Speed: " <> show state.speedControl)
        ]
        <>
        [ SE.svg
            [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
            [ SE.g []
            (drawCube edges vert2Ds)
            ]
        ]
    where
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