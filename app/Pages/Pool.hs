module Pages.Pool where

import Imports

import qualified Data.Map as M (insert, (!?), delete, toList, singleton, member)
import qualified Data.Set as S (insert, delete)

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
    poolName <- arg "name"
    poolId <- insert Pool {poolProblems=mempty, poolMembers=M.singleton userId Owner, ..}
    return $ Just $ "/pool/"<>show poolId<>"/problems") $(template "pool_create")

addParticipant :: App Response
addParticipant = withUser \inviter _ -> do
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
