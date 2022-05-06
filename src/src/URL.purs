module URL (resolveURL) where

import Prelude
import Data.String (trim, take)

-- given a URL, eg. from artist code, resolve it to
-- a complete URL
resolveURL :: String -> String
resolveURL x = f $ trim x where
  f x | take 7 x == "http://" = x
      | take 8 x == "https://" = x
      | otherwise = defaultAssets <> x

defaultAssets :: String
defaultAssets = "https://dktr0.github.io/LocoMotion/models/"
