module Handler.Message where


import Import


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- fmap (fmap entityVal) $ runDB $ selectList [MessageGroupId ==. groupId] [Asc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId message
        person <- runDB $ get404 personId
        return (message, person)

    renderWithGroups $(widgetFile "message/list") "グループチャット" groupId


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    personId <- requireAuthId
    now <- liftIO getNow
    _ <- runDB $ insert $ Message "hoge-" now groupId personId
    redirect $ MessageListR groupId
