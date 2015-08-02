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


getBelongRemoveR :: GroupId -> Handler Html
getBelongRemoveR groupId = do
    authId <- requireAuthId
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId, BelongPersonId ==. authId]
    redirect $ HomeR
