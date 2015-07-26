module Handler.Home where


import Import

import Database.Persist.Sql(fromSqlKey)

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
    events <- fmap (fmap entityVal) $ runDB $ selectList [EventGroupId ==. groupId] [Desc EventId]

    contents <- forM events $ \event -> do
        let personId = eventPersonId event
        person <- runDB $ get404 personId
        return (event, person)

    renderWithGroups $(widgetFile "home/home") "ホーム" PHome ["ホーム"] groupId []
