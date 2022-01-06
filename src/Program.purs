module Program where

import Data.Maybe (Maybe)
import Data.Number (fromString)

type Program = Number -- for now, a valid program is simply a number controlling speed of rotation

defaultProgram :: Program
defaultProgram = 0.0

parseProgram :: String -> Maybe Program
parseProgram = fromString
