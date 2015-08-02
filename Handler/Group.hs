module Handler.Group where


import Import


postGroupCreateR :: Handler Html
postGroupCreateR = do
    ((res, _), _) <- runFormPost $ fGroup
    case res of
        FormSuccess entity -> do
            groupId <- runDB $ insert entity

            now <- liftIO getCurrentTime
            authId <- requireAuthId
            _ <- runDB $ insert $ Belong now now groupId authId False

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

    mBelong <- runDB $ selectFirst [BelongGroupId ==. groupId, BelongPersonId ==. authId] [Asc BelongId]

    now <- liftIO getCurrentTime

    case mBelong of
        (Just _) -> do
            _ <- runDB $ updateWhere [BelongGroupId ==. groupId, BelongPersonId ==. authId] [BelongUpdated =. now, BelongDeleted =. False]
            redirect $ HomeWithGroupIdR groupId

        Nothing  -> do
            _ <- runDB $ insert $ Belong now now groupId authId False
            redirect $ HomeWithGroupIdR groupId


getBelongDeleteR :: GroupId -> Handler Html
getBelongDeleteR groupId = do
    now <- liftIO getCurrentTime
    authId <- requireAuthId
    _ <- runDB $ updateWhere [BelongGroupId ==. groupId, BelongPersonId ==. authId] [BelongUpdated =. now, BelongDeleted =. True]
    redirect $ HomeR


getGroupDeleteR :: GroupId -> Handler Html
getGroupDeleteR groupId = do
    _ <- runDB $ deleteWhere [GroupId ==. groupId]
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId]

    redirect $ HomeR
