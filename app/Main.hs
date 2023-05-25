module Main where

import Prelude ()

import Imports
import Pages.Auth
import Pages.Navigation
import Pages.Pool

main :: IO ()
main = runApp $ do
    askRq >>= log . show
    msum
        [ dir "login" login
        , dir "register" register
        , dir "logout" logout
        , dir "feed" feed
        , dir "contests" contests
        , dir "collections" collections
        , dir "editor" editor
        , dir "accept_pool_invitation" acceptPoolInvitation
        , dir "decline_pool_invitation" declinePoolInvitation
        , dir "pool" $ path pool
        , dirs "pools/create" createPool
        , dir "add_participant" addParticipant
        , dir "remove_participant" removeParticipant
        , dir "static" $ serveDirectory DisableBrowsing [] "static"
        ]
