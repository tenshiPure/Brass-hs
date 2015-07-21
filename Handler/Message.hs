module Handler.Message where


import Import
import Data.Time



getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- fmap (fmap entityVal) $ runDB $ selectList [MessageGroupId ==. groupId] [Asc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId message
        person <- runDB $ get404 personId
        return (message, person)

    tz <- liftIO getCurrentTimeZone

    renderWithGroups $(widgetFile "message/list") "チャット" PMessage groupId [$(widgetFile "widget/media")]
        


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    personId <- requireAuthId
    mBody <- lookupPostParam "body"

    case mBody of
        (Just body) -> do
            now <- liftIO getCurrentTime
            _ <- runDB $ insert $ Message body now groupId personId
            redirect $ MessageListR groupId
        Nothing -> redirect $ MessageListR groupId
