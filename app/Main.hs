{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import App

main :: IO ()
main = runApp $ msum
    [ dir "signin" signin
    , dir "profile" $ msum
        [ path profile
        , dir "new" newProfile
        , dir "edit" editProfile
        ]
    , currentUser >>= maybe newProfile profile
    ]

signin :: App Response
signin = form ["email", "pass"] (\[email, pass] -> [page|
<form action="/signin" method="post">
    <label for="email">Email:</label>
    <input type="email" name="email" required="required" ("value" ?= email)/>
    <br/>
    
    <label for="pass">Пароль:</label>
    <input type="password" name="pass" required="required" ("value" ?= pass)/>
    <br/>

    <input type="submit" value="Войти"/>
    <a href="/profile/new">Регистрация</a>
</form>
|]) \[email, pass] -> do
    Just userId <- query email
    Just User{passwordHash} <- query @(Id User) userId
    guard $ checkPass pass passwordHash

    newCookie "userId" $ show userId
    newCookie "pass" $ unpack pass
    
    return $ "/profile/"<>show userId

profile :: Id User -> App Response
profile n = query n >>=
    maybe (notFound "User with given id doesn't exist") \User{..} -> currentUser >>= \u -> [page|
<h1><% name %></h1>
<p><% email %></p>
<% if u == Just n then <% <a href="/profile/edit">Редактировать</a> %> else <% () %> %>
|]

newProfile :: App Response
newProfile = form ["name", "email", "pass1", "pass2"] (\[name, email, pass1, pass2] -> [page|
<form action="/profile/new" method="post">
    <label for="name">Имя:</label>
    <input type="text" name="name" required="required" ("value" ?= name)/>
    <br/>

    <label for="email">Email:</label>
    <input type="email" name="email" required="required" ("value" ?= email)/>
    <br/>
    
    <label for="pass1">Пароль:</label>
    <input type="password" name="pass1" required="required" ("value" ?= pass1)/>
    <br/>
    
    <label for="pass2">Повторите пароль:</label>
    <input type="password" name="pass2" required="required" ("value" ?= pass2)/>
    <br/>

    <input type="submit" value="Зарегистрироваться"/>
</form>
|]) \[name, email, pass1, pass2] ->do
    query email >>= guard . isNothing @(Id User)
    guard $ pass1 == pass2 && App.length pass1 >= 8
    
    passwordHash <- hashPass pass1
    User{userId} <- insert $ \userId -> User{..}
    update email userId

    newCookie "userId" $ show userId
    newCookie "pass" $ unpack pass1
    
    return $ "/profile/"<>show userId

editProfile :: App Response
editProfile = withUser \User{..} ->
    form ["newName", "newEmail"] (\[newName, newEmail] -> [page|
<form action="/profile/edit" method="post">
    <label for="name">Имя:</label>
    <input type="text" name="newName" required="required" value=(fromMaybe name newName)/>
    <br/>

    <label for="email">Email:</label>
    <input type="email" name="newEmail" required="required" value=(fromMaybe email newEmail)/>
    <br/>

    <input type="submit" value="Сохранить"/>
</form>
|]) \[newName, newEmail] -> do
    when (email /= newEmail) do
        old <- query newEmail
        guard $ isNothing @(Id User) old
        delete @_ @(Id User) email
        update newEmail userId

    update userId User{email = newEmail, name = newName, ..}
    
    return $ "/profile/"<>show userId
