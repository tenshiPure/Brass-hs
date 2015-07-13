module Handler.Debug where


import Import
import Handler.Attendance(fromInt, presenceTypes, fAttendance)


fPerson :: Maybe Person -> Form Person
fPerson mPerson = renderDivs $ Person
    <$> areq textField "メール" (personMail <$> mPerson)
    <*> areq textField "名前"   (personName <$> mPerson)


getDebugPersonCreateR :: Handler Html
getDebugPersonCreateR = do
    (widget, enctype) <- generateFormPost (fPerson Nothing)

    defaultLayout $(widgetFile "debug/person/create")


postDebugPersonCreateR :: Handler Html
postDebugPersonCreateR = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            personId <- runDB $ insert person
            redirect $ PersonDetailR personId

        _ -> defaultLayout $(widgetFile "debug/person/create")
            


getDebugPersonUpdateR :: PersonId -> Handler Html
getDebugPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ (fPerson $ Just person)

    defaultLayout $(widgetFile "person/update")
        


postDebugPersonUpdateR :: PersonId -> Handler Html
postDebugPersonUpdateR personId = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            runDB $ replace personId person
            redirect $ PersonDetailR personId

        _ -> defaultLayout $(widgetFile "person/update")
            


postDebugPersonDeleteR :: PersonId -> Handler Html
postDebugPersonDeleteR personId = do
    runDB $ delete personId
    redirect $ PersonListR





getDebugAttendanceCreateR :: GroupId -> ScheduleId -> PersonId -> Handler Html
getDebugAttendanceCreateR groupId scheduleId personId = do
    (widget, enctype) <- generateFormPost (fAttendance scheduleId personId)

    defaultLayout $(widgetFile "attendance/create")


postDebugAttendanceCreateR :: GroupId -> ScheduleId -> PersonId -> Handler Html
postDebugAttendanceCreateR groupId scheduleId personId = do
    ((res, _), _) <- runFormPost (fAttendance scheduleId personId)
    case res of
        FormSuccess attendance -> do
            _ <- runDB $ insert attendance
            redirect $ ScheduleDetailR groupId scheduleId

        _ -> redirect $ ScheduleDetailR groupId scheduleId
