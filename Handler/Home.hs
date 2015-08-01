module Handler.Home where


import Import

import Model.Event

getHomeR :: Handler Html
getHomeR = do
    personId <- requireAuthId

    belongs <- runDB $ selectList [BelongPersonId ==. personId] [Asc BelongId]
    let belongGroupIds = map (belongGroupId . entityVal) belongs
    mGroup <- runDB $ selectFirst [GroupId <-. belongGroupIds] [Asc GroupId]

    case mGroup of
        (Just topGroup) -> redirect $ HomeWithGroupIdR (entityKey topGroup)
        Nothing      -> error "no join group"


getHomeWithGroupIdR :: GroupId -> Handler Html
getHomeWithGroupIdR groupId = do
    messages <- runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageId]
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Desc LinkId]
    comments <- runDB $ selectList [] [Desc CommentId]

    messageEventLogs <- mapM messageToEventLog messages
    linkEventLogs <- mapM linkToEventLog links
    commentEventLogs <- mapM commentToEventLog comments

    let contents = sortBy (\x y -> compare (now x) (now y)) $ messageEventLogs ++ linkEventLogs ++ commentEventLogs

    renderWithGroups $(widgetFile "home/event") "ホーム" PHome groupId [$(widgetFile "widget/media")]
