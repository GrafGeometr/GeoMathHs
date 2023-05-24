{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import App
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runApp $ msum
    [ dir "register" register
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    ]

base :: EmbedAsChild (HSPT XML App) p => p -> HTML XML
base content = do
    current_user <- liftHTML $ currentUser >>= maybe (pure Nothing) query
    $(html "base" False)

register :: App Response
register = msum
    [ method GET >> $(template "register")
    , method POST >> do
        json "email" >>= liftIO . putStrLn
        ok (toResponse "/register")
    ]
