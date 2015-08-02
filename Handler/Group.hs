module Handler.Group where


import Import


postGroupCreateR :: Handler Html
postGroupCreateR = do
    ((res, _), _) <- runFormPost $ fGroup
    case res of
        FormSuccess entity -> do
            groupId <- runDB $ insert entity

            authId <- requireAuthId
            _ <- runDB $ insert $ Belong groupId authId 

            redirect $ HomeWithGroupIdR groupId

        _ -> error "todo"


getGroupManageR :: GroupId -> Handler Html
getGroupManageR groupId = do
    error $ "todo" ++ (show groupId)


getGroupUpdateR :: GroupId -> Handler Html
getGroupUpdateR groupId = do
    error $ "todo" ++ (show groupId)


postGroupUpdateR :: GroupId -> Handler Html
postGroupUpdateR groupId = do
    error $ "todo" ++ (show groupId)


getGroupInvitedR :: GroupId -> Handler Html
getGroupInvitedR groupId = do
    authId <- requireAuthId
    _ <- runDB $ insert $ Belong groupId authId

    redirect $ HomeWithGroupIdR groupId


getBelongDeleteR :: GroupId -> Handler Html
getBelongDeleteR groupId = do
    authId <- requireAuthId
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId, BelongPersonId ==. authId]
    redirect $ HomeR


getGroupDeleteR :: GroupId -> Handler Html
getGroupDeleteR groupId = do
    _ <- runDB $ deleteWhere [GroupId ==. groupId]
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId]

    redirect $ HomeR
