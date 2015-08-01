module Model.Event where


import Import


data EventLog = MessageEventLog { groupId :: GroupId, person :: Person, messageId :: MessageId, body  :: Text, created :: UTCTime }
              | LinkEventLog    { groupId :: GroupId, person :: Person, linkId    :: LinkId, title :: Text, created :: UTCTime }
              | CommentEventLog { groupId :: GroupId, person :: Person, linkId :: LinkId, title :: Text, commentId :: CommentId, body :: Text , created :: UTCTime }


messageToEventLog :: Entity Message -> HandlerT App IO EventLog
messageToEventLog message = do
    person <- runDB $ get404 (messagePersonId $ entityVal message)

    return $ MessageEventLog (messageGroupId $ entityVal message) person (entityKey message) (cut 50 (messageBody $ entityVal message)) (messageCreated $ entityVal message)


linkToEventLog :: Entity Link -> HandlerT App IO EventLog
linkToEventLog link = do
    person <- runDB $ get404 (linkPersonId $ entityVal link)

    return $ LinkEventLog (linkGroupId $ entityVal link) person (entityKey link) (linkTitle $ entityVal link) (linkCreated $ entityVal link)


commentToEventLog :: Entity Comment -> HandlerT App IO EventLog
commentToEventLog comment = do
    person <- runDB $ get404 (commentPersonId $ entityVal comment)

    let linkId = commentLinkId $ entityVal comment
    link <- runDB $ get404 linkId

    return $ CommentEventLog (commentGroupId $ entityVal comment) person linkId (linkTitle link) (entityKey comment) (cut 30 (commentBody $ entityVal comment)) (commentCreated $ entityVal comment)


cut :: Int -> Textarea -> Text
cut n body = if length text < n
             then text
             else take n text ++ "..."
    where
        text = unTextarea body
