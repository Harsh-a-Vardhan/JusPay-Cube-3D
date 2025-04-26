module Cube where

import Data.Tuple
import Effect.Random
import Prelude

import Data.Array (cons, drop, insert, length, mapWithIndex, reverse, (!!), dropEnd)
import Data.Array.NonEmpty (findLastIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int
import Effect.Class.Console (info, logShow)
import Effect.Console (log)
import Effect.Exception (stack)
import Halogen (query)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HEL
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label)
import Halogen.Svg.Attributes (FillState(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Math (cos, sin)
import Prim.Boolean (True)
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

type Count=Int



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
  , myval::Number
  , id::Int
  
  
  }




data Axis = X | Y | Z

-- Model / State
type State = Array Cube

--type MyState=Array Cube

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


myvalue::Number
myvalue = 50.0

tenDegInRad :: Angle
tenDegInRad = oneDegInRad * 10.0

accelerateBy :: Number
accelerateBy = oneDegInRad * myvalue



update::Number->Number
update c=c*2.0

deupdate::Number->Number
deupdate c=c/2.0


dampenPercent :: Number
dampenPercent = 1.0 - (0.9 / frameRate) -- 10% per second


myinit::Array Cube
myinit=[initCube] 

randIdGen::Int->Int
randIdGen a=a+1  

initCube :: Cube
initCube =
  { 
    shape:
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
  , myval :1.0
  , id: 0

  }



data Query a = Tick a | Other a

-- Events
data Action
  = DecAngVelocity Axis
  | IncAngVelocity Axis
  | Reverse Int
  | SpeedUp Int
  | SpeedDown Int
  | Add
  | RemoveIt
  | GetScore
  


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
        initialState = myinit

        render :: forall m. State -> H.ComponentHTML Action () m
        render state = HH.div[][HH.ul[]$map renderView state]

        runFunction :: _ -> H.HalogenM State Action () output m Unit
        runFunction fn = do
          _ <- H.modify fn
          pure unit


 
        handleAction :: Action -> H.HalogenM State Action () output m Unit
        handleAction query = case query of
            DecAngVelocity axis -> runFunction   (  map \c -> decAngVelocity axis c ) 
            IncAngVelocity axis -> runFunction  (map \c -> incAngVelocity axis c)
            Reverse id->runFunction (map \c ->reversecube c id)

            SpeedUp id ->runFunction (map \c ->speedup c id)
            SpeedDown id ->runFunction (map \c ->speeddown c id)
            Add-> runFunction(\c->addValue initCube c )
            RemoveIt->runFunction(\c->removeValue c)
            GetScore->runFunction(map \c-> getscore c)
            

        handleQuery :: forall m a message. Query a -> H.HalogenM State Action () message m (Maybe a)
        handleQuery = case _ of
          Tick a -> do
            _ <- H.modify (map \c -> tick c)
            pure (Just a)
          Other a -> 
            pure (Just a)




removeValue::Array Cube->Array Cube
removeValue arr= dropEnd 1 arr
          

addValue::Cube->Array Cube->Array Cube
addValue val arr= insert val{id=length arr } arr

reversecube::Cube->Int->Cube
reversecube c id=  if c.id == id then c{forward= if (c.forward==true) then false else true} else c
     
incAngVelocity :: Axis -> Cube -> Cube
incAngVelocity axis c = do 
  
  let {xa, ya, za} = c.angVel

  case axis of
    
    X -> c { angVel { xa = if (c.forward==true) then xa + (accelerateBy+c.myval) else xa-(accelerateBy+c.myval) } }
    Y -> c { angVel { ya = if (c.forward==true) then ya + (accelerateBy+c.myval) else ya-(accelerateBy+c.myval) } }
    Z -> c { angVel { za = if (c.forward==true) then za + (accelerateBy+c.myval) else za-(accelerateBy+c.myval) } }


speedup :: Cube->Int->Cube
speedup c id=if c.id==id then c{myval=c.myval+0.5} else c


speeddown :: Cube->Int->Cube
speeddown c id=if c.id==id then c{myval=c.myval-0.5} else c



decAngVelocity :: Axis -> Cube -> Cube
decAngVelocity axis c = do 
   
  let {xa, ya, za} = c.angVel
  case axis of
    X -> c { angVel { xa = xa - accelerateBy } }
    Y -> c { angVel { ya = ya - accelerateBy } }
    Z -> c { angVel { za = za - accelerateBy } }





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


getscore::Cube->Cube
getscore c=c

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
       
        

        if state.id==0 then


        HH.div [] $
        [ renderText  "Please Click on ADD  button"
        , renderButton "rotX++" (IncAngVelocity X)
        , renderButton "rotY++" (IncAngVelocity Y)
        , renderButton "rotZ++" (IncAngVelocity Z)
        
        , renderButton "add" (Add)
        , renderButton "remove" (RemoveIt)
       
        
        ]
        
        



        else


        HH.div [] $
        [ 
         renderText  getstring 
         , renderButton "reverse" (Reverse state.id)
        , renderButton "speed++" (SpeedUp state.id)
        , renderButton "speed--" (SpeedDown state.id)
        
        
        
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
        renderText label=
            HH.h1 [ ] [ HH.text label ]

        -- parallel projection
        project :: Point3D -> Point2D
        project p =
            { x: p.x + viewCenter.x
            , y: p.y + viewCenter.y
            }
        getstring::String
        getstring = toStringAs decimal state.id

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