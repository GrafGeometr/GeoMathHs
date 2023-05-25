module Pages.Auth where

import Imports

import Data.Set (singleton)

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
    userName <- arg "login"
    email <- arg "email"
    password <- arg "password"
    repeat_password <- arg "repeat_password"

    guard $ password == repeat_password

    userPasswordHash <- hashPass password
    now <- liftIO getCurrentTime
    i <- insert User{userEmails = singleton email, userPools = mempty, userDateCreated = now, ..}
    log $ "Registred: "<>show i

    update userName i  

    createToken email EmailInfo {emailVerificationToken=Nothing, emailUser=i, emailDateCreated=now}
    loginUser i password
    
    return $ Just "/feed") $(template "register")

logout :: App Response
logout = do
    logoutUser
    redirect "/login"
