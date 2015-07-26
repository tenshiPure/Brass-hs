module Handler.Schedule where


import Import
import Handler.Attendance(fromInt)


getScheduleListR :: GroupId -> Handler Html
getScheduleListR groupId = do
    contents <- runDB $ selectList [ScheduleGroupId ==. groupId] [Asc ScheduleId]

    (formWidget, enctype) <- generateFormPost (fSchedule groupId)

    renderWithGroups $(widgetFile "schedule/list") "予定 一覧" PSchedule ["予定 一覧"] groupId [$(widgetFile "widget/no-image-list")]


getScheduleDetailR :: GroupId -> ScheduleId -> Handler Html
getScheduleDetailR groupId scheduleId = do
    schedule <- runDB $ get404 scheduleId

    attendances <- runDB $ selectList [AttendanceScheduleId ==. scheduleId] [Asc AttendanceId]
    let personIds = map (attendancePersonId . entityVal) attendances
    persons <- mapM (runDB . get404) personIds

    let contents = zip attendances persons

    renderWithGroups $(widgetFile "schedule/detail") "予定 詳細" PSchedule ["予定 一覧", "予定 詳細"] groupId []


fSchedule :: GroupId -> Html -> MForm Handler (FormResult Schedule, Widget)
fSchedule groupId extra = do
    (dayResult, dayView)         <- mreq textField (createSettings "schedule-form__day"   [("placeholder", "日付を入力")]) Nothing
    (placeResult, placeView)     <- mopt textField (createSettings "schedule-form__place" [("placeholder", "場所を入力（任意）")]) Nothing
    (noteResult, noteView)       <- mopt textField (createSettings "schedule-form__note"  [("placeholder", "備考を入力（任意）")]) Nothing
    (groupIdResult, groupIdView) <- mreq hiddenField "" (Just groupId)
    let result = Schedule
           <$> dayResult
           <*> placeResult
           <*> noteResult
           <*> groupIdResult
        widget = $(widgetFile "schedule/form/schedule")
    return (result, widget)


postScheduleCreateR :: GroupId -> Handler Html
postScheduleCreateR groupId = do
    ((res, formWidget), enctype) <- runFormPost (fSchedule groupId)
    case res of
        FormSuccess schedule -> do
            _ <- runDB $ insert schedule
            redirect $ ScheduleListR groupId

        _ -> error "todo"
