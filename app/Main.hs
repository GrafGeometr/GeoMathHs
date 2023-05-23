{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import App

main :: IO ()
main = return ()

base :: EmbedAsChild (HSPT XML App) p => p -> XMLGenT (HSPT XML App) XML
base content = do
    current_user <- liftHTML $ currentUser >>= maybe (pure Nothing) query
    $(html "base" False)

register :: App XML
register = $(template "register")
