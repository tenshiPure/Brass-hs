module Handler.Message where


import Import
import Data.Time



getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- fmap (fmap entityVal) $ runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageCreated, Desc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId message
        person <- runDB $ get404 personId
        return (message, person)

    tz <- liftIO getCurrentTimeZone

    renderWithGroups $(widgetFile "message/list") "チャット" PMessage ["チャット"] groupId [$(widgetFile "widget/media")]
        


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    mBody <- lookupPostParam "body"

    case mBody of
        (Just body) -> do
            personId <- requireAuthId
            now <- liftIO getCurrentTime

            messageId <- runDB $ insert $ Message body now groupId personId

            writeEvent body groupId personId messageId

            redirect $ MessageListR groupId
        Nothing -> redirect $ MessageListR groupId


writeEvent :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> GroupId -> PersonId -> MessageId -> HandlerT site IO ()
writeEvent body groupId personId messageId = do
    now <- liftIO getCurrentTime
    _ <- runDB $ insert $ Event (take 25 body) now groupId personId (Just messageId) Nothing Nothing Nothing Nothing
    return ()
