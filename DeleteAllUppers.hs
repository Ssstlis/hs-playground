module DeleteAllUppers where

import Data.Char ( isUpper )

delAllUpper :: String -> String
delAllUpper = unwords . filter ( not . all isUpper) . words