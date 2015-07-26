module Handler.Message where


import Import
import Data.Time
import Database.Persist.Sql(fromSqlKey)


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId $ entityVal message
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

            writeEvent 1 (pack $ show $ fromSqlKey messageId) (body) "" "" groupId personId

            redirect $ MessageListR groupId
        Nothing -> redirect $ MessageListR groupId
