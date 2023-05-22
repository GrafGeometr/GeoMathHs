{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import App

main :: IO ()
main = return ()

base :: EmbedAsChild (HSPT XML App) p => p -> XMLGenT (HSPT XML App) XML
base content = $(html "base" False)

register :: App XML
register = $(template "register")
