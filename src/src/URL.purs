module URL (resolveURL) where

import Prelude
import Data.String (trim, take, toLower)

-- given a URL, eg. from artist code, resolve it to
-- a complete URL
resolveURL :: String -> String
resolveURL x = f $ trim x where
  f x | take 7 x == "http://" = x
      | take 8 x == "https://" = x
      | toLower x == "cactus" = defaultAssets <> "cactus.glb"
      | toLower x == "daffy" = defaultAssets <> "Daffy.glb"
      | toLower x == "lily" = defaultAssets <> "Lily.glb"
      | toLower x == "naturegirl" = defaultAssets <> "NatureGirl.glb"
      | toLower x == "stonefigure" = defaultAssets <> "StoneFigure.glb"
      | toLower x == "willy" = defaultAssets <> "Willy.glb"
      | toLower x == "woman-nl" = defaultAssets <> "Woman-NLA.glb"
      | toLower x == "ant" = defaultAssets <> "ant.glb"
      | toLower x == "branch" = defaultAssets <> "Branch.glb"
      | toLower x == "crackman" = defaultAssets <> "crackman.glb"
      | toLower x == "diver" = defaultAssets <> "Diver.glb"
      | toLower x == "fossegrim" = defaultAssets <> "fossegrim.glb"
      | toLower x == "leafy" = defaultAssets <> "leafy.glb"
      | toLower x == "oak" = defaultAssets <> "Oak.glb"
      | toLower x == "raccoon" = defaultAssets <> "raccoon.glb"
      | toLower x == "wireman" = defaultAssets <> "wireman.glb"
      | toLower x == "soldier" = "https://threejs.org/examples/models/gltf/Soldier.glb"
      | otherwise = defaultAssets <> x

defaultAssets :: String
defaultAssets = "https://dktr0.github.io/LocoMotion/models/"
