{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Prelude ()

import App
import qualified Data.Map as M (insert, (!?), delete, toList, singleton, member)
import qualified Data.Set as S (singleton, insert, delete)

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
        , dir "add_participant" addParticipiant
        , dir "remove_participant" removeParticipant
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
    i <- insert User{userEmails = S.singleton email, userPools = mempty, userDateCreated = now, ..}
    log $ "Registred: "<>show i

    createToken email EmailInfo {emailVerificationToken=Nothing, emailUser=i, emailDateCreated=now}
    loginUser i password
    
    return $ Just "/feed") $(template "register")

logout :: App Response
logout = do
    logoutUser
    redirect "/login"

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

acceptPoolInvitation :: App Response
acceptPoolInvitation = withUser \i _ -> do
    method POST
    poolId :: Id Pool <- json "pool_hashed_id"
    tryQuery poolId \pool -> case poolMembers pool M.!? i of
        Just Invited -> do
            update poolId $ pool & _poolMembers %~ M.insert i Participant
            ok $ toResponse ()
        Just _ -> notFound $ toResponse "user arledy is participant"
        Nothing -> notFound $ toResponse "user is not invited"

declinePoolInvitation :: App Response
declinePoolInvitation = withUser \i _ -> do
    method POST
    poolId :: Id Pool <- json "pool_hashed_id"
    tryQuery poolId \pool -> case poolMembers pool M.!? i of
        Just Invited -> do
            update poolId $ pool & _poolMembers %~ M.delete i
            ok $ toResponse ()
        Just _ -> notFound $ toResponse "user arledy is participant"
        Nothing -> notFound $ toResponse "user is not invited"

poolHeader :: Id Pool -> Text -> HTML [ChildType (HSPT XML App)]
poolHeader poolId poolName = do
    Just current_user <- liftHTML $ currentUser >>= maybe (pure Nothing) query
    $(html "pool_header" True)

pool :: Id Pool -> App Response
pool poolId = withUser \userId _ -> tryQuery poolId \Pool{..} ->
    guard (userId `M.member` poolMembers) >> msum
    [ dir "problems" $(template "pool_problems")
    , dir "participants" do
        members <- forM (M.toList poolMembers) \(id, role) -> do
            Just User{userName} <- query id
            return (userName, role)
        $(template "pool_participants")
    , dir "management" do
        $(template "pool_management")
    ]

createPool :: App Response
createPool = withUser \userId _ -> form (do
    poolName <- json "name"
    poolId <- insert Pool {poolProblems=mempty, poolMembers=M.singleton userId Owner, ..}
    return $ Just $ "/pool/"<>show poolId<>"/problems") $(template "pool_create")

addParticipiant :: App Response
addParticipiant = withUser \inviter _ -> do
    method POST
    name :: UserName <- json "login"
    poolId <- json "pool_hashed_id"

    Just userId <- query name
    Just user <- query userId
    Just pool <- query poolId
    guard $ inviter `M.member` poolMembers pool
    
    update userId $ user & _userPools %~ S.insert poolId
    update poolId $ pool & _poolMembers %~ M.insert userId Invited

    ok $ toResponse ()

removeParticipant :: App Response
removeParticipant = withUser \remover _ -> do
    method POST
    name :: UserName <- json "login"
    poolId <- json "pool_hashed_id"

    Just userId <- query name
    Just user <- query userId
    Just pool <- query poolId
    guard $ remover `M.member` poolMembers pool
    
    update userId $ user & _userPools %~ S.delete poolId
    update poolId $ pool & _poolMembers %~ M.delete userId

    ok $ toResponse ()
