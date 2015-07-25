module Handler.Schedule where


import Import
import Handler.Attendance(fromInt)


getScheduleListR :: GroupId -> Handler Html
getScheduleListR groupId = do
    contents <- runDB $ selectList [ScheduleGroupId ==. groupId] [Asc ScheduleId]

    renderWithGroups $(widgetFile "schedule/list") "予定 一覧" PSchedule ["予定 一覧"] groupId [$(widgetFile "widget/no-image-list")]


getScheduleDetailR :: GroupId -> ScheduleId -> Handler Html
getScheduleDetailR groupId scheduleId = do
    schedule <- runDB $ get404 scheduleId

    attendances <- runDB $ selectList [AttendanceScheduleId ==. scheduleId] [Asc AttendanceId]
    let personIds = map (attendancePersonId . entityVal) attendances
    persons <- mapM (runDB . get404) personIds

    let contents = zip attendances persons

    renderWithGroups $(widgetFile "schedule/detail") "予定 詳細" PSchedule ["予定 一覧", "予定 詳細"] groupId []


fSchedule :: GroupId -> Maybe Schedule -> Form Schedule
fSchedule groupId mSchedule = renderDivs $ Schedule
    <$> areq dayField    "日程" (scheduleDay   <$> mSchedule)
    <*> aopt textField   "場所" (schedulePlace <$> mSchedule)
    <*> aopt textField   "備考" (scheduleNote  <$> mSchedule)
    <*> areq hiddenField ""     (Just groupId)


getScheduleCreateR :: GroupId -> Handler Html
getScheduleCreateR groupId = do
    (widget, enctype) <- generateFormPost (fSchedule groupId Nothing)

    defaultLayout $(widgetFile "schedule/create")


postScheduleCreateR :: GroupId -> Handler Html
postScheduleCreateR groupId = do
    ((res, widget), enctype) <- runFormPost (fSchedule groupId Nothing)
    case res of
        FormSuccess schedule -> do
            _ <- runDB $ insert schedule
            redirect $ ScheduleListR groupId

        _ -> defaultLayout $(widgetFile "schedule/create")


getScheduleUpdateR :: GroupId -> ScheduleId -> Handler Html
getScheduleUpdateR groupId scheduleId = do
    schedule <- runDB $ get404 scheduleId
    (widget, enctype) <- generateFormPost (fSchedule groupId $ Just schedule)

    defaultLayout $(widgetFile "schedule/update")


postScheduleUpdateR :: GroupId -> ScheduleId -> Handler Html
postScheduleUpdateR groupId scheduleId = do
    ((res, widget), enctype) <- runFormPost (fSchedule groupId Nothing)
    case res of
        FormSuccess schedule -> do
            runDB $ replace scheduleId schedule
            redirect $ ScheduleListR groupId

        _ -> defaultLayout $(widgetFile "schedule/create")
