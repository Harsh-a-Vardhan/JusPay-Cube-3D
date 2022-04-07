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
  , accelerationRate
  , anglePerFrame
  , cubes
  , dampenAngVelocity
  , dampenPercent
  , decAcceleration
  , frameRate
  , incAcceleration
  , incAngVelOfCube
  , incAngVelocity
  , initCube
  , oneDegInRad
  , renderView
  , reverseRotation
  , rotate
  , rotateShape
  , tenDegInRad
  , viewCenter
  )
  where
import Data.List
import Data.Tuple
import Prelude
import Control.Comonad.Store (pos)
import Data.Array (mapWithIndex, modifyAt, deleteAt, snoc, (!!))
import Data.Array.NonEmpty (reverse)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Debug (spy)
import Halogen as H
import Data.String
import Halogen.HTML (input)
import Halogen.HTML as HH
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (m, y)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Math (cos, sin)
import Web.HTML.History (forward)
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
  , velFactor :: Number
  , forward :: Boolean
  }
data Axis = X | Y | Z
-- Model / State
type State = Array Cube
-- Values
viewBoxSize :: Number
viewBoxSize = 500.0
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
accelerationRate :: Number
accelerationRate = 1.0
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
      , velFactor: 1.0
  , forward: true
  }
data Query a = Tick a | Other a
-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis
  | IncAcceleration Int
  | DecAcceleration Int
  | ReverseRotation Int
  | RemoveCube Int
  | AddCube
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
        render = renderView
        runFunction :: _ -> H.HalogenM State Action () output m Unit
        runFunction fn = do
          _ <- H.modify fn
          pure unit
        handleAction :: Action -> H.HalogenM State Action () output m Unit
        handleAction query = case query of
            DecAngVelocity axis -> H.modify_ \state -> state
            IncAngVelocity axis -> runFunction  (\c -> incAngVelocity axis c)
            ReverseRotation pos -> runFunction (\c -> reverseRotation c pos)
            IncAcceleration pos -> runFunction (\c -> incAcceleration c pos)
            DecAcceleration pos -> runFunction (\c -> decAcceleration c pos)
            RemoveCube pos -> runFunction (\c -> removeCube c pos)
            AddCube -> runFunction (\c -> addCube c)
        handleQuery :: forall m a message. Query a -> H.HalogenM State Action () message m (Maybe a)
        handleQuery = case _ of
          Tick a -> do
            _ <- H.modify (\c -> tick c)
            pure (Just a)
          Other a -> 
            pure (Just a)
     
incAngVelocity :: Axis -> Array Cube -> Array Cube
incAngVelocity axis lc = map(\c -> incAngVelOfCube axis c) lc
  
incAngVelOfCube :: Axis -> Cube -> Cube
incAngVelOfCube axis c = do 
  let {xa, ya, za} = c.angVel
  case axis of
      X -> c { angVel { xa = xa + accelerateBy } }
      Y -> c { angVel { ya = ya + accelerateBy } }
      Z -> c { angVel { za = za + accelerateBy } }
incAcceleration :: Array Cube -> Int -> Array Cube
incAcceleration cubes pos = do
  let
    newState = modifyAt pos (\c -> c {velFactor = c.velFactor + accelerationRate}) cubes
  -- let p = head cubes
  let p = 10
  let _ = spy "xyz" p
  maybe [] (\c -> c) newState
decAcceleration :: Array Cube -> Int -> Array Cube
decAcceleration cubes pos = do
  let
    newState = modifyAt pos (\c -> c {velFactor = (getMax (c.velFactor - accelerationRate) 0.0)}) cubes
  maybe [] (\c -> c) newState
getMax :: Number -> Number -> Number
getMax x y = if x > y then x else y
reverseRotation ::Array Cube -> Int -> Array Cube
reverseRotation cubes pos = do
  let
    newState = modifyAt pos (\c -> c {forward = c.forward /= true}) cubes
  maybe [] (\c -> c) newState
removeCube ::Array Cube -> Int -> Array Cube
removeCube cubes pos = do
  let
    newState = deleteAt pos cubes
  maybe [] (\c -> c) newState
addCube :: Array Cube -> Array Cube
addCube cubes = snoc cubes initCube
  
tick :: Array Cube -> Array Cube
tick ca =  map(\c -> updateCube c) ca
updateCube :: Cube -> Cube
updateCube c = do
  let angVel = c.angVel
      {vertices, edges} = c.shape
      newShape =
        { edges : edges
        , vertices: rotateShape vertices (anglePerFrame c)
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
anglePerFrame :: Cube -> Angle3D
anglePerFrame c = do
  let {xa, ya, za} = c.angVel
  let direction = if c.forward then 1.0 else -1.0
  { xa: xa / frameRate * direction * (c.velFactor)
  , ya: ya / frameRate * direction * (c.velFactor)
  , za: za / frameRate * direction * (c.velFactor)
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
renderView :: forall m. Array Cube -> H.ComponentHTML Action () m
renderView state = HH.div [] $
        [ renderButton "rotX++" (IncAngVelocity X)
        , renderButton "rotY++" (IncAngVelocity Y)
        , renderButton "rotZ++" (IncAngVelocity Z)
        , renderButton "add cube" (AddCube)
        ]
        <>
        mapWithIndex(\pos cube -> let
          {vertices, edges} = cube.shape
          vert2Ds = map project vertices
          in
          HH.div [] $
            [SE.svg
              [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
              [ SE.g []
              (drawCube edges vert2Ds)
              ]
            ]
            <>
            [ renderButton "inc speed" (IncAcceleration pos)
            , renderButton "dec speed" (DecAcceleration pos)
            , renderButton "reverse" (ReverseRotation pos)
            , renderButton "remove" (RemoveCube pos)
            ]
        ) state
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