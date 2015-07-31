module Handler.Home where


import Import
import Database.Persist.Sql(toSqlKey, fromSqlKey)

getHomeR :: Handler Html
getHomeR = do
    personId <- requireAuthId

    belongs <- runDB $ selectList [BelongPersonId ==. personId] [Asc BelongId]
    let belongGroupIds = map (belongGroupId . entityVal) belongs
    mGroup <- runDB $ selectFirst [GroupId <-. belongGroupIds] [Asc GroupId]

    case mGroup of
        (Just topGroup) -> redirect $ HomeWithGroupIdR (entityKey topGroup)
        Nothing      -> error "no join group"


getHomeWithGroupIdR :: GroupId -> Handler Html
getHomeWithGroupIdR groupId = do
    messages <- runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageId]
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Desc LinkId]
    comments <- runDB $ selectList [] [Desc CommentId]



    messageEventLogs <- mapM messageToEventLog messages
    linkEventLogs <- mapM linkToEventLog links
    commentEventLogs <- mapM commentToEventLog comments

    let eventLogs = sortBy (\x y -> compare (now x) (now y)) $ messageEventLogs ++ linkEventLogs ++ commentEventLogs

    defaultLayout [whamlet|
        $forall eventLog <- eventLogs
            $case eventLog
                $of MessageEventLog groupId person messageId body _
                    <p>
                        #{personName person}がチャットで
                        <a href=@{MessageListR groupId}#message-#{messageId}>
                            #{body}
                        と発言しました
                $of LinkEventLog groupId person linkId title _
                    <p>
                        #{personName person}がリンクに
                        <a href=@{LinkListR groupId}#link-#{linkId}>
                            #{title}
                        を作成しました
                $of CommentEventLog groupId person linkId' linkId title commentId body _
                    <p>
                        #{personName person}がリンク
                        <a href=@{LinkListR groupId}#link-#{linkId}>
                            #{title}
                        に
                        <a href=@{LinkDetailR groupId linkId'}#comment-#{commentId}>
                            #{body}
                        とコメントしました
    |]

--     tz <- liftIO getCurrentTimeZone

--     renderWithGroups $(widgetFile "home/home") "ホーム" PHome groupId [$(widgetFile "widget/media")]


data EventLog = MessageEventLog { groupId :: GroupId, person :: Person, messageId :: Int64, body  :: Text, now :: UTCTime }
              | LinkEventLog    { groupId :: GroupId, person :: Person, linkId    :: Int64, title :: Text, now :: UTCTime }
              | CommentEventLog { groupId :: GroupId, person :: Person, linkId' :: LinkId, linkId    :: Int64, title :: Text, commentId :: Int64, body :: Text , now :: UTCTime }


messageToEventLog :: Entity Message -> HandlerT App IO EventLog
messageToEventLog message = do
    now <- liftIO getCurrentTime
    person <- runDB $ get404 (messagePersonId $ entityVal message)
    return $ MessageEventLog (messageGroupId $ entityVal message) person (fromSqlKey $ entityKey message) (messageBody $ entityVal message) now


linkToEventLog :: Entity Link -> HandlerT App IO EventLog
linkToEventLog link = do
    now <- liftIO getCurrentTime
    person <- runDB $ get404 (linkPersonId $ entityVal link)
    return $ LinkEventLog (linkGroupId $ entityVal link) person (fromSqlKey $ entityKey link) (linkTitle $ entityVal link) now


commentToEventLog :: Entity Comment -> HandlerT App IO EventLog
commentToEventLog comment = do
    now <- liftIO getCurrentTime
    person <- runDB $ get404 (commentPersonId $ entityVal comment)
    link <- runDB $ get404 (commentLinkId $ entityVal comment)
    let groupId = toSqlKey 1 :: GroupId
    let linkId = toSqlKey 1 :: LinkId

    return $ CommentEventLog groupId person linkId (fromSqlKey $ commentLinkId $ entityVal comment) (linkTitle link) (fromSqlKey $ entityKey comment) (unTextarea $ commentBody $ entityVal comment) now
