module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Timer as T
import Effect.Class (liftEffect)
import Effect.Console (log)

import Cube as C
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

frameRate :: Int
frameRate = 200


main :: Effect Unit
main = HA.runHalogenAff do
  -- y <- Just "world"
  -- logShow "Hello"
  body <- HA.awaitBody
  cube <- runUI C.cubes unit body
  
  liftEffect $ T.setInterval (1000 / frameRate) do
    HA.runHalogenAff $ cube.query $ H.mkTell C.Tick

  
