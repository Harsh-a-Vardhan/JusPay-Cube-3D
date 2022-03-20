module Cube where

import Prelude

import Data.Tuple
import Data.Array (mapWithIndex, (!!), snoc, drop)
import Data.Maybe (Maybe(..), fromMaybe)


import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Properties as HP

import Math (cos, sin)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

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
  , counter :: Number
  , velocity :: Number
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

velChange :: Number
velChange = oneDegInRad + 5.0

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
  , counter: 1.0
  , velocity: 1.0
  }

data Query a = Tick a | Other a

-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis
  | Reverse Number
  | IncVel Number
  | DecVel Number
  | AddCube
  | DropCube


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
            Reverse id -> runFunction (\c -> map (reverse id) c)
            IncVel id -> runFunction (\c -> map (incVel id) c)
            DecVel id -> runFunction (\c -> map (decVel id) c)
            AddCube -> runFunction (\c -> snoc (map updateCounter c) initCube)
            DropCube -> runFunction (\c -> drop 1 c)

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
  let velocity = c.velocity
  let dir = c.forward
  case axis of
    X -> c { angVel { xa = if dir then xa + accelerateBy + velocity else xa - accelerateBy - velocity} }
    Y -> c { angVel { ya = if dir then ya + accelerateBy + velocity else ya - accelerateBy - velocity} }
    Z -> c { angVel { za = if dir then za + accelerateBy + velocity else za - accelerateBy - velocity} }



reverse ::  Number -> Cube -> Cube
reverse id c = c {forward = 
                  if id == c.counter then not c.forward
                  else c.forward}

incVel :: Number -> Cube -> Cube
incVel id c = do
  let {xa, ya, za} = c.angVel
  let velocity = c.velocity
  c { velocity = 
        if id == c.counter then velocity + 5.0 
        else velocity}

decVel :: Number -> Cube -> Cube
decVel id c = do
  let {xa, ya, za} = c.angVel
  let velocity = c.velocity
  c { velocity = 
        if id == c.counter then 
          if velocity - 5.0 > 0.0 
            then velocity - 5.0
          else 1.0
        else velocity}        

updateCounter :: Cube ->  Cube
updateCounter c = do 
  let count = c.counter
  c {counter = count + 1.0}

    

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
        , renderButton "reverse" (Reverse state.counter)
        , renderButton "incVel" (IncVel state.counter)
        , renderButton "decVel" (DecVel state.counter)
        , renderButton "addCube" (AddCube)
        , renderButton "dropCube" (DropCube)
        , HH.text (show state.forward)
        , HH.text (show state.counter)
        , HH.text (show state.velocity) 
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