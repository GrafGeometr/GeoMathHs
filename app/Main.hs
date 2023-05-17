module Main where

import Happstack.Lite

main :: IO ()
main = serve Nothing $
    ok $ toResponse "Geomath"
