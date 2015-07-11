module Handler.Schedule where


import Import


getScheduleListR :: GroupId -> Handler Html
getScheduleListR groupId = do
    schedules <- runDB $ selectList [] [Asc ScheduleId]

    defaultLayout $(widgetFile "schedule/list")


fSchedule :: Maybe Schedule -> Form Schedule
fSchedule mSchedule = renderDivs $ Schedule
    <$> areq dayField  "日程" (scheduleDay   <$> mSchedule)
    <*> aopt textField "場所" (schedulePrace <$> mSchedule)
    <*> aopt textField "備考" (scheduleNote  <$> mSchedule)


getScheduleCreateR :: GroupId -> Handler Html
getScheduleCreateR groupId = do
    (widget, enctype) <- generateFormPost (fSchedule Nothing)

    defaultLayout $(widgetFile "schedule/create")


postScheduleCreateR :: GroupId -> Handler Html
postScheduleCreateR groupId = do
    ((res, widget), enctype) <- runFormPost (fSchedule Nothing)
    case res of
        FormSuccess schedule -> do
            _ <- runDB $ insert schedule
            redirect $ ScheduleListR groupId

        _ -> defaultLayout $ do
            $(widgetFile "schedule/create")
