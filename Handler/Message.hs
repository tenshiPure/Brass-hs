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

            _ <- runDB $ insert $ Message body now groupId personId

            writeEvent "やったー" groupId personId

            redirect $ MessageListR groupId
        Nothing -> redirect $ MessageListR groupId


writeEvent :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> GroupId -> PersonId -> HandlerT site IO ()
writeEvent content groupId personId = do
    now <- liftIO getCurrentTime
    _ <- runDB $ insert $ Event content now groupId personId
    return ()
