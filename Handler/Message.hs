module Handler.Message where


import Import


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- fmap (fmap entityVal) $ runDB $ selectList [MessageGroupId ==. groupId] [Asc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId message
        person <- runDB $ get404 personId
        return (message, person)

    renderWithGroups $(widgetFile "message/list") "グループチャット" PMessage groupId


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    personId <- requireAuthId
    mBody <- lookupPostParam "body"

    case mBody of
        (Just body) -> do
            now <- liftIO getNow
            _ <- runDB $ insert $ Message body now groupId personId
            redirect $ MessageListR groupId
        Nothing -> redirect $ MessageListR groupId
