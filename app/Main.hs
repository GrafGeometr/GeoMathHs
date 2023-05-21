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
    , dir "problem" $ msum
        [ path problem
        , dir "new" newProblem
        ]
    , dir "archive" archive
    , nullDir >> seeOther ("/archive" :: String) (toResponse ())
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
profile n = tryQuery n \User{..} -> currentUser >>= \u -> [page|
<h1><% name %></h1>
<p><% email %></p>
<% if u == Just n then <% <%>
    <a href="/profile/edit">Редактировать</a>
    <br/>
    <a href="/problem/new">Создать задачу</a>
</%> %> else <% () %> %>
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
    userId <- insert User{..}
    update email userId

    newCookie "userId" $ show userId
    newCookie "pass" $ unpack pass1
    
    return $ "/profile/"<>show userId

editProfile :: App Response
editProfile = withUser \userId User{..} ->
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

tagsXML :: [Tag] -> XMLGenT (HSPT XML App) XML
tagsXML xs = [hsx|
<table>
    <tr>
        <% map (\tag -> <th><% tagName tag %></th>) xs %>
    </tr>
</table>
|]

problemXML :: Problem -> XMLGenT (HSPT XML App) [ChildType (HSPT XML App)] -> XMLGenT (HSPT XML App) [ChildType (HSPT XML App)]
problemXML Problem{..} sol = [hsx|
<%>
    <h1>Условие</h1>
    <p><% condition %></p>
    <% sol %>
    <% tagsXML problemTags %>
</%>
|]

problem :: Id Problem -> App Response
problem n = tryQuery n \p -> msum
    [ nullDir >> [page|
<% problemXML p
    <%>
        <a href=("/problem/"<>pack (show n)<>"/solution")>Решение</a>
    </%>
%> |]
    , dir "solution" [page|
<% problemXML p
    <%>
        <h1>Решение</h1>
        <p><% solution p %></p>
    </%>
%> |]
    ]

newProblem :: App Response
newProblem = form ["condition", "solution", "tags"] (\[condition, solution, tags] -> [page|
<form action="/problem/new" method="POST">
    <label for="condition">Условие</label>
    <input type="text" name="condition" required="required" ("value" ?= condition)/>
    <br/>

    <label for="solution">Решение</label>
    <input type="text" name="solution" required="required" ("value" ?= solution)/>
    <br/>

    <label for="tags">Теги</label>
    <input type="text" name="tags" required="required" ("value" ?= tags)/>
    <br/>

    <input type="submit" value="Создать"/>
</form>
|]) \[condition, solution, tags] -> do
    problemId <- insert Problem{problemTags = Tag <$> splitOn " " tags, ..}
    time <- liftIO getCurrentTime
    insertOrdered (time, problemId)
    return $ "/problem/"<>show problemId

archive :: App Response
archive = do
    user <- currentUser
    time <- addUTCTime (-nominalDay*30) <$> liftIO getCurrentTime
    problems <- queryManyOrdered (\(posted, problemId) -> if posted < time
        then return Nothing
        else fmap (problemId,) <$> query @(Id Problem) problemId)
    [page|
<a href=(maybe "/signin" (\u -> "/profile/"<>pack (show u)) user)>Профиль</a>
<h1>Архив задач за последний месяц</h1>
<% traverse (\(i, p) -> problemXML p <%>
    <a href=("/problem/"<>pack (show i)<>"/solution")>Решение</a>
</%>) problems %>
|]
