module Model.Event where


import Import


data EventLog = BelongEventLog     { person :: Person, icon :: Text, body :: Text, at :: UTCTime }
              | MessageEventLog    { groupId :: GroupId, person :: Person, messageId :: MessageId, body :: Text, at :: UTCTime }
              | ScheduleEventLog   { groupId :: GroupId, person :: Person, scheduleId :: ScheduleId, day :: Text, at :: UTCTime }
              | AttendanceEventLog { groupId :: GroupId, person :: Person, scheduleId :: ScheduleId, day :: Text, attendanceId :: AttendanceId, presence :: Int , at :: UTCTime }
              | LinkEventLog       { groupId :: GroupId, person :: Person, linkId :: LinkId, title :: Text, at :: UTCTime }
              | CommentEventLog    { groupId :: GroupId, person :: Person, linkId :: LinkId, title :: Text, commentId :: CommentId, body :: Text , at :: UTCTime }


belongToEventLog :: Entity BelongLog -> HandlerT App IO EventLog
belongToEventLog belongLog = do
    person <- runDB $ get404 (belongLogPersonId $ entityVal belongLog)

    return $ case (belongLogAction $ entityVal belongLog) of
        0 -> BelongEventLog person "chat_1.png" "追加" (belongLogCreated $ entityVal belongLog)
        _ -> BelongEventLog person "chat_2.png" "削除" (belongLogCreated $ entityVal belongLog)


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


scheduleToEventLog :: Entity Schedule -> HandlerT App IO EventLog
scheduleToEventLog schedule = do
    person <- runDB $ get404 (schedulePersonId $ entityVal schedule)

    return $ ScheduleEventLog (scheduleGroupId $ entityVal schedule) person (entityKey schedule) (scheduleDay $ entityVal schedule) (scheduleCreated $ entityVal schedule)


attendanceToEventLog :: Entity Attendance -> HandlerT App IO EventLog
attendanceToEventLog attendance = do
    person <- runDB $ get404 (attendancePersonId $ entityVal attendance)

    let scheduleId = attendanceScheduleId $ entityVal attendance
    schedule <- runDB $ get404 scheduleId

    return $ AttendanceEventLog (attendanceGroupId $ entityVal attendance) person scheduleId (scheduleDay $ schedule) (entityKey attendance) (attendancePresence $ entityVal attendance) (attendanceCreated $ entityVal attendance)


cut :: Int -> Textarea -> Text
cut n body = if length text < n
             then text
             else take n text ++ "..."
    where
        text = unTextarea body
