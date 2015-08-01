module Handler.Schedule where


import Import
import Database.Persist.Sql(fromSqlKey)


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


getScheduleListR :: GroupId -> Handler Html
getScheduleListR groupId = do
    contents <- runDB $ selectList [ScheduleGroupId ==. groupId] [Asc ScheduleId]

    authId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fSchedule groupId authId

    renderWithGroups $(widgetFile "schedule/list") "予定 一覧" PSchedule groupId [$(widgetFile "widget/no-image-list")]


getScheduleDetailR :: GroupId -> ScheduleId -> Handler Html
getScheduleDetailR groupId scheduleId = do
    schedule <- runDB $ get404 scheduleId

    attendances <- runDB $ selectList [AttendanceScheduleId ==. scheduleId] [Asc AttendanceId]
    let personIds = map (attendancePersonId . entityVal) attendances
    persons <- mapM (runDB . get404) personIds

    let contents = zip attendances persons

    personId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fAttendance scheduleId groupId personId

    renderWithGroups $(widgetFile "schedule/detail") "予定 詳細" PSchedule groupId []


fSchedule :: GroupId -> PersonId -> Html -> MForm Handler (FormResult Schedule, Widget)
fSchedule groupId personId = renderDivs $ Schedule
    <$> areq textField (createSettings "schedule-form__day"   [("placeholder", "日付を入力")])         Nothing
    <*> aopt textField (createSettings "schedule-form__place" [("placeholder", "場所を入力（任意）")]) Nothing
    <*> aopt textField (createSettings "schedule-form__note"  [("placeholder", "備考を入力（任意）")]) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq hiddenField "" (Just groupId)
    <*> areq hiddenField "" (Just personId)


fAttendance :: ScheduleId -> GroupId -> PersonId -> Html -> MForm Handler (FormResult Attendance, Widget)
fAttendance scheduleId groupId personId = renderDivs $ Attendance
    <$> areq (selectFieldList presenceTypes) (createSettings "attendance-form__presence" [])                                      Nothing
    <*> aopt textField                       (createSettings "attendance-form__note"     [("placeholder", "備考を入力（任意）")]) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq hiddenField "" (Just scheduleId)
    <*> areq hiddenField "" (Just groupId)
    <*> areq hiddenField "" (Just personId)


postScheduleCreateR :: GroupId -> Handler Html
postScheduleCreateR groupId = do
    authId <- requireAuthId
    ((res, _), _) <- runFormPost $ fSchedule groupId authId
    case res of
        FormSuccess schedule -> do
            scheduleId <- runDB $ insert schedule

            personId <- requireAuthId
            writeEvent 2 (pack $ show $ fromSqlKey scheduleId) (fromMaybe "" $ schedulePlace schedule) "" "" groupId personId

            redirect $ ScheduleListR groupId

        _ -> error "todo"


postScheduleAttendanceCreateR :: GroupId -> ScheduleId -> Handler Html
postScheduleAttendanceCreateR groupId scheduleId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost $ fAttendance scheduleId groupId personId
    case res of
        FormSuccess attendance -> do
            attendanceId <- runDB $ insert attendance

            schedule <- runDB $ get404 scheduleId
            writeEvent 3 (pack $ show $ fromSqlKey scheduleId) (fromMaybe "" $ schedulePlace schedule) (pack $ show $ fromSqlKey attendanceId) (fromInt $ attendancePresence attendance) groupId personId

            redirect $ ScheduleDetailR groupId scheduleId

        _ -> error "todo"
