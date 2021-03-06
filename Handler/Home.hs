module Handler.Home where


import Import

import Model.Event
import Handler.Schedule(fromInt)
import Data.Time

getHomeR :: Handler Html
getHomeR = do
    personId <- requireAuthId

    belongs <- runDB $ selectList [BelongPersonId ==. personId] [Asc BelongId]
    let belongGroupIds = map (belongGroupId . entityVal) belongs
    mGroup <- runDB $ selectFirst [GroupId <-. belongGroupIds] [Asc GroupId]

    case mGroup of
        (Just topGroup) -> redirect $ HomeWithGroupIdR (entityKey topGroup)
        Nothing      -> redirect $ HomeWelcomeR


getHomeWithGroupIdR :: GroupId -> Handler Html
getHomeWithGroupIdR groupId = do
    groupLogs   <- runDB $ selectList [GroupLogGroupId ==. groupId]   [Desc GroupLogId]
    belongLogs  <- runDB $ selectList [BelongLogGroupId ==. groupId]  [Desc BelongLogId]
    messages    <- runDB $ selectList [MessageGroupId ==. groupId]    [Desc MessageId]
    schedules   <- runDB $ selectList [ScheduleGroupId ==. groupId]   [Desc ScheduleId]
    attendances <- runDB $ selectList [AttendanceGroupId ==. groupId] [Desc AttendanceId]
    links       <- runDB $ selectList [LinkGroupId ==. groupId]       [Desc LinkId]
    comments    <- runDB $ selectList [CommentGroupId ==. groupId]    [Desc CommentId]

    groupEventLogs      <- mapM groupToEventLog      groupLogs
    belongEventLogs     <- mapM belongToEventLog     belongLogs
    messageEventLogs    <- mapM messageToEventLog    messages
    scheduleEventLogs   <- mapM scheduleToEventLog   schedules
    attendanceEventLogs <- mapM attendanceToEventLog attendances
    linkEventLogs       <- mapM linkToEventLog       links
    commentEventLogs    <- mapM commentToEventLog    comments

    let contents = sortBy (\x y -> compare (getAt y) (getAt x)) $ groupEventLogs ++ belongEventLogs ++ messageEventLogs ++ scheduleEventLogs ++ attendanceEventLogs ++ linkEventLogs ++ commentEventLogs

    tz <- liftIO getCurrentTimeZone

    renderWithGroups $(widgetFile "home/event") "ホーム" PHome groupId [$(widgetFile "widget/media")]


getAt :: (Person, UTCTime, EventLog) -> UTCTime
getAt (_, at, _) = at


getHomeWelcomeR :: Handler Html
getHomeWelcomeR = do
    defaultLayout $ do
        setTitle "ようこそ"
        $(widgetFile "home/welcome")
