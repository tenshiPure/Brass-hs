module Handler.Attendance where


import Import


fAttendance :: ScheduleId -> PersonId -> Form Attendance
fAttendance scheduleId personId = renderDivs $ Attendance
    <$> areq hiddenField "" (Just personId)
    <*> areq hiddenField "" (Just scheduleId)


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
