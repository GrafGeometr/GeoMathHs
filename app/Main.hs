{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Prelude ()

import App
import Data.Set (singleton)

main :: IO ()
main = runApp $ do
    askRq >>= log . show
    msum
        [ dir "login" login
        , dir "register" register
        , dir "feed" feed
        , dir "contests" contests
        , dir "collections" collections
        , dir "editor" editor
        , dir "static" $ serveDirectory DisableBrowsing [] "static"
        ]

base :: EmbedAsChild (HSPT XML App) p => p -> HTML XML
base content = do
    current_user <- liftHTML $ currentUser >>= maybe (pure Nothing) query
    $(html "base" False)

login :: App Response
login = form (do
    name :: UserName <- arg "login"
    password <- arg "password"
    next <- optional $ argStr "next"
    Just i <- query name
    Just User{..} <- query i
    if checkPass password userPasswordHash
        then do
            loginUser i password
            return . Just $ fromMaybe "/myprofile" next
        else do
            log "Incorrect password"
            return Nothing) -- TODO: Flash message
    $(template "login")

register :: App Response
register = form (do
    userName <- json "login"
    email <- json "email"
    password <- json "password"
    repeat_password <- json "repeat_password"

    guard $ password == repeat_password

    userPasswordHash <- hashPass password
    now <- liftIO getCurrentTime
    i <- insert User{userEmails = singleton email, userPools = mempty, userDateCreated = now, ..}
    log $ "Registred: "<>show i

    createToken email EmailInfo {emailVerificationToken=Nothing, emailUser=i, emailDateCreated=now}
    loginUser i password
    
    return $ Just "/feed") $(template "register")

feed :: App Response
feed = do
    $(template "feed")

contests :: App Response
contests = do
    $(template "contests")

collections :: App Response
collections = do
    $(template "collections")

editor :: App Response
editor = do
    $(template "editor")

