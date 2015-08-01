module Model.Event where


import Import
import Database.Persist.Sql(toSqlKey, fromSqlKey)


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
