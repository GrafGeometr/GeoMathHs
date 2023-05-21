{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import App
import qualified Data.Set as S (insert)

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
        , dir "new" $ newProblem Nothing
        ]
    , dir "archive" archive
    , dir "pool" $ msum
        [ path pool
        , dir "new" newPool
        ]
    , nullDir >> seeOther ("/archive" :: String) (toResponse ())
    ]

signin :: App Response
signin = form ["email", "pass"] (\[email, pass] -> [page|
<form method="post">
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
<% if u == Just n then <% do
pools <- query n
<%>
    <% (\(p :: Id Pool) -> <%><a href=("/pool/"<>pack (show p))><% show p %></a><br/></%>) <$> fromMaybe [] pools %>

    <a href="/profile/edit">Редактировать</a>
    <br/>
    <a href="/problem/new">Создать задачу</a>
    <br/>
    <a href="/pool/new">Новый пул</a>
</%> %> else <% () %> %>
|]

newProfile :: App Response
newProfile = form ["name", "email", "pass1", "pass2"] (\[name, email, pass1, pass2] -> [page|
<form method="post">
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
<form method="post">
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

tagsXML :: Set Tag -> XMLGenT (HSPT XML App) XML
tagsXML xs = [hsx|
<table>
    <tr>
        <% map (\tag -> <th><% tagName tag %></th>) $ toList xs %>
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

problemXMLWithSolutionRef :: Id Problem -> Problem -> XMLGenT (HSPT XML App) [ChildType (HSPT XML App)]
problemXMLWithSolutionRef n p = problemXML p [hsx|
<%>
    <a href=("/problem/"<>pack (show n)<>"/solution")>Решение</a>
</%>
|]

userXML :: Id User -> XMLGenT (HSPT XML App) [ChildType (HSPT XML App)]
userXML n = [hsx|
<%>
    <a href=("/profile/"<>pack (show n))><% maybe "Unknown" name <$> query n :: XMLGenT (HSPT XML App) Text %></a>
    <br/>
</%>
|]

problem :: Id Problem -> App Response
problem n = tryQuery n \p -> let
    response = msum
        [ nullDir >> [page| <% problemXMLWithSolutionRef n p %> |]
        , dir "solution" [page|
    <% problemXML p
        <%>
            <h1>Решение</h1>
            <p><% solution p %></p>
        </%>
    %> |]
        ] in
    query @_ @(Id Pool) n >>= maybe response (`tryQuery` \Pool{..} -> withUser \userId _ -> do
        guard $ userId `member` _poolEditAccess
        response)

newProblem :: Maybe (Id Pool, Pool) -> App Response
newProblem source = form ["condition", "solution", "tags"] (\[condition, solution, tags] -> [page|
<form method="POST">
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
    problemId <- insert Problem{problemTags = fromList $ Tag <$> splitOn " " tags, ..}
    maybe (do
        time <- liftIO getCurrentTime
        insertOrdered (time, problemId)) (\(poolId, p) -> do
            update problemId poolId
            p & poolProblems %~ S.insert problemId & update poolId) source
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
<% uncurry problemXMLWithSolutionRef <$> problems %>
|]

pool :: Id Pool -> App Response
pool n = withUser \userId _ -> tryQuery n \Pool{..} -> if userId `member` _poolEditAccess
    then msum
        [ do
            nullDir
            problems <- traverse (\p -> (p,) <$> query p) $ toList _poolProblems
            [page|
<h1>Пул задач</h1>
<h2>Владелец</h2>
<% userXML _poolOwner %>
<h2>Могут редактировать</h2>
<% userXML <$> toList _poolEditAccess %>
<a href=("/pool/"<>pack (show n)<>"/addUser")>Добавить</a>
<h1>Задачи</h1>
<% uncurry (\p -> maybe (<%><p>Problem not found: <% pack $ show p %></p></%>) $ problemXMLWithSolutionRef p) <$> problems %>
<a href=("/pool/"<>pack (show n)<>"/addProblem")>Добавить</a>
|]
        , dir "addUser" $ form ["userId"] (\[newUserId] -> [page|
<form method="post">
    <label for="userId">ID пользователя</label>
    <input type="number" name="userId" required="required" ("value" ?= newUserId)/>
    <br/>

    <input type="submit" value="Добавить"/>
</form>
|]) \[newUser] -> do
            Just newUserId <- return . readMaybe $ unpack newUser
            Pool{..} & poolEditAccess %~ S.insert newUserId & update n
            pools <- query newUserId
            update _poolOwner $ n : fromMaybe [] pools
            return $ "/pool/"<>show n
        , dir "addProblem" $ newProblem $ Just (n, Pool{..})
        ]
    else unauthorized "You have no access to this pool"

newPool :: App Response
newPool = withUser \_poolOwner _ -> do
    poolId <- insert Pool{_poolProblems = mempty, _poolEditAccess = singleton _poolOwner, ..}
    pools <- query _poolOwner
    update _poolOwner $ poolId : fromMaybe [] pools
    seeOther ("/pool/"<>show poolId) $ toResponse ()
