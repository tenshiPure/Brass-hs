module Handler.Attendance where


import Import


presenceTypes :: [(Text, Int)]
presenceTypes = [(fromInt 1, 1),
                 (fromInt 2, 2),
                 (fromInt 3, 3),
                 (fromInt 4, 4)]

fromInt :: Int -> Text
fromInt n = case n of
    1 -> "参加"
    2 -> "欠席"
    3 -> "遅刻"
    4 -> "早退"
    _ -> ""


fAttendance :: ScheduleId -> PersonId -> Form Attendance
fAttendance scheduleId personId = renderDivs $ Attendance
    <$> areq hiddenField                     ""     (Just personId)
    <*> areq hiddenField                     ""     (Just scheduleId)
    <*> areq (selectFieldList presenceTypes) "出欠" (Nothing)
    <*> aopt textField                       "備考" (Nothing)


fAttendance' :: Maybe Attendance -> Form Attendance
fAttendance' mAttendance = renderDivs $ Attendance
    <$> areq hiddenField                     ""     (attendancePersonId <$> mAttendance)
    <*> areq hiddenField                     ""     (attendanceScheduleId <$> mAttendance)
    <*> areq (selectFieldList presenceTypes) "出欠" (attendancePresence <$> mAttendance)
    <*> aopt textField                       "備考" (attendanceNote <$> mAttendance)


getAttendanceCreateR :: GroupId -> ScheduleId -> Handler Html
getAttendanceCreateR groupId scheduleId = do
    personId <- requireAuthId
    (widget, enctype) <- generateFormPost (fAttendance scheduleId personId)

    defaultLayout $(widgetFile "attendance/create")


postAttendanceCreateR :: GroupId -> ScheduleId -> Handler Html
postAttendanceCreateR groupId scheduleId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost (fAttendance scheduleId personId)
    case res of
        FormSuccess attendance -> do
            _ <- runDB $ insert attendance
            redirect $ ScheduleDetailR groupId scheduleId

        _ -> redirect $ ScheduleDetailR groupId scheduleId


getAttendanceUpdateR :: GroupId -> ScheduleId -> AttendanceId -> Handler Html
getAttendanceUpdateR groupId scheduleId attendanceId = do
    attendance <- runDB $ get404 attendanceId
    (widget, enctype) <- generateFormPost (fAttendance' $ Just attendance)

    defaultLayout $(widgetFile "attendance/update")


postAttendanceUpdateR :: GroupId -> ScheduleId -> AttendanceId -> Handler Html
postAttendanceUpdateR groupId scheduleId attendanceId = do
    ((res, widget), enctype) <- runFormPost (fAttendance' Nothing)
    case res of
        FormSuccess attendance -> do
            runDB $ replace attendanceId attendance
            redirect $ ScheduleDetailR groupId scheduleId

        _ -> redirect $ ScheduleDetailR groupId scheduleId
