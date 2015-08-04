module Model.Event where


import Import


data EventLog = GroupEventLog      { body :: Text, icon :: Text }
              | BelongEventLog     { icon :: Text, body :: Text }
              | MessageEventLog    { groupId :: GroupId, messageId :: MessageId, body :: Text }
              | ScheduleEventLog   { groupId :: GroupId, scheduleId :: ScheduleId, day :: Text }
              | AttendanceEventLog { groupId :: GroupId, scheduleId :: ScheduleId, day :: Text, attendanceId :: AttendanceId, presence :: Int }
              | LinkEventLog       { groupId :: GroupId, linkId :: LinkId, title :: Text }
              | CommentEventLog    { groupId :: GroupId, linkId :: LinkId, title :: Text, commentId :: CommentId, body :: Text }


groupToEventLog :: Entity GroupLog -> HandlerT App IO (Person, UTCTime, EventLog)
groupToEventLog groupLog = do
    person <- runDB $ get404 $ groupLogPersonId $ entityVal groupLog

    return $ case entityVal groupLog of
        GroupLog name icon 0 created groupId _ -> (person, created, GroupEventLog (name ++ " を作成しました") icon)
        GroupLog name icon 1 created groupId _ -> (person, created, GroupEventLog (name ++ " に変更しました") icon)


belongToEventLog :: Entity BelongLog -> HandlerT App IO (Person, UTCTime, EventLog)
belongToEventLog belongLog = do
    person <- runDB $ get404 $ belongLogPersonId $ entityVal belongLog

    return $ case entityVal belongLog of
        BelongLog 0 created groupId _ -> (person, created, BelongEventLog "in.png"  "グループに参加しました")
        BelongLog 1 created groupId _ -> (person, created, BelongEventLog "out.png" "グループから退席しました")


messageToEventLog :: Entity Message -> HandlerT App IO (Person, UTCTime, EventLog)
messageToEventLog message = do
    let messageId = entityKey message
    person <- runDB $ get404 $ messagePersonId $ entityVal message

    return $ case entityVal message of
        Message body created groupId _ -> (person, created, MessageEventLog groupId messageId (cut 50 body))


scheduleToEventLog :: Entity Schedule -> HandlerT App IO (Person, UTCTime, EventLog)
scheduleToEventLog schedule = do
    let scheduleId = entityKey schedule
    person <- runDB $ get404 $ schedulePersonId $ entityVal schedule

    return $ case entityVal schedule of
        Schedule day _ _ created groupId _-> (person, created, ScheduleEventLog groupId scheduleId day)


attendanceToEventLog :: Entity Attendance -> HandlerT App IO (Person, UTCTime, EventLog)
attendanceToEventLog attendance = do
    let attendanceId = entityKey attendance
    person <- runDB $ get404 $ attendancePersonId $ entityVal attendance
    schedule <- runDB $ get404 $ attendanceScheduleId $ entityVal attendance

    return $ case entityVal attendance of
        Attendance presence _ created scheduleId groupId _ ->  (person, created, AttendanceEventLog groupId scheduleId (scheduleDay $ schedule) attendanceId presence)


linkToEventLog :: Entity Link -> HandlerT App IO (Person, UTCTime, EventLog)
linkToEventLog link = do
    let linkId = entityKey link
    person <- runDB $ get404 $ linkPersonId $ entityVal link

    return $ case entityVal link of
        Link title _ _ created groupId _ -> (person, created, LinkEventLog groupId linkId title)


commentToEventLog :: Entity Comment -> HandlerT App IO (Person, UTCTime, EventLog)
commentToEventLog comment = do
    let commentId = entityKey comment
    person <- runDB $ get404 (commentPersonId $ entityVal comment)
    link <- runDB $ get404 $ commentLinkId $ entityVal comment

    return $ case entityVal comment of
        Comment body created linkId groupId _ -> (person, created, CommentEventLog groupId linkId (linkTitle link) commentId (cut 30 body))


cut :: Int -> Textarea -> Text
cut n body = if length text < n
             then text
             else take n text ++ "..."
    where
        text = unTextarea body
