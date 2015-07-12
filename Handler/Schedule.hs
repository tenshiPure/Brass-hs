module Handler.Schedule where


import Import


getScheduleListR :: GroupId -> Handler Html
getScheduleListR groupId = do
    entity <- runDB $ get404 groupId
    schedules <- runDB $ selectList [ScheduleGroupId ==. groupId] [Asc ScheduleId]

    defaultLayout $(widgetFile "schedule/list")


getScheduleDetailR :: GroupId -> ScheduleId -> Handler Html
getScheduleDetailR groupId scheduleId = do
    entity <- runDB $ get404 groupId
    schedule <- runDB $ get404 scheduleId

    defaultLayout $(widgetFile "schedule/detail")


fSchedule :: GroupId -> Maybe Schedule -> Form Schedule
fSchedule groupId mSchedule = renderDivs $ Schedule
    <$> areq dayField    "日程" (scheduleDay   <$> mSchedule)
    <*> aopt textField   "場所" (schedulePrace <$> mSchedule)
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
