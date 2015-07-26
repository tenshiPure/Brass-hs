module Handler.Home where


import Import


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
    contents <- fmap (fmap entityVal) $ runDB $ selectList [EventGroupId ==. groupId] [Desc EventCreated, Desc EventId]
    renderWithGroups $(widgetFile "home/home") "ホーム" PHome ["ホーム"] groupId []
