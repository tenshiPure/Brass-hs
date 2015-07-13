module Handler.Message where


import Import


fMessage :: Maybe Message -> GroupId -> PersonId -> Form Message
fMessage mMessage groupId personId = renderDivs $ Message
    <$> areq textField "内容" (messageBody <$> mMessage)
    <*> areq dayField  "時間" (messageCreateAt <$> mMessage)
    <*> areq hiddenField "" (Just personId)
    <*> areq hiddenField "" (Just groupId)


renderWithGroups :: Widget -> GroupId -> Handler Html
renderWithGroups content currentGroupId = do
    personId <- requireAuthId
    person <- runDB $ get404 personId

    belongs <- runDB $ selectList [BelongPersonId ==. personId] [Asc BelongId]
    let belongGroupIds = map (belongGroupId . entityVal) belongs
    groups <- runDB $ selectList [GroupId <-. belongGroupIds] [Asc GroupId]

    defaultLayout $(widgetFile "message/wrapper")


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- runDB $ selectList [] [Asc MessageId]

    renderWithGroups $(widgetFile "message/list") groupId


getMessageCreateR :: GroupId -> Handler Html
getMessageCreateR groupId = do
    personId <- requireAuthId
    (widget, enctype) <- generateFormPost (fMessage Nothing groupId personId)

    defaultLayout $(widgetFile "message/create")


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost (fMessage Nothing groupId personId)
    case res of
        FormSuccess belong -> do
            _ <- runDB $ insert belong
            redirect $ MessageListR groupId
        _ -> do
            redirect $ MessageListR groupId
