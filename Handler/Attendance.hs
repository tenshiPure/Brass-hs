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


getAttendanceCreateR :: GroupId -> ScheduleId -> PersonId -> Handler Html
getAttendanceCreateR groupId scheduleId personId = do
    (widget, enctype) <- generateFormPost (fAttendance scheduleId personId)

    defaultLayout $(widgetFile "attendance/create")


postAttendanceCreateR :: GroupId -> ScheduleId -> PersonId -> Handler Html
postAttendanceCreateR groupId scheduleId personId = do
    ((res, _), _) <- runFormPost (fAttendance scheduleId personId)
    case res of
        FormSuccess attendance -> do
            _ <- runDB $ insert attendance
            redirect $ ScheduleDetailR groupId scheduleId

        _ -> redirect $ ScheduleDetailR groupId scheduleId
