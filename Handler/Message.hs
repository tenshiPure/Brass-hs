module Handler.Message where


import Import


fMessage :: GroupId -> PersonId -> Form Message
fMessage groupId personId = renderDivs $ Message
    <$> areq textField "内容" Nothing
    <*> areq dayField  "時間" Nothing
    <*> areq hiddenField "" (Just personId)
    <*> areq hiddenField "" (Just groupId)


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    messages <- fmap (fmap entityVal) $ runDB $ selectList [MessageGroupId ==. groupId] [Asc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId message
        person <- runDB $ get404 personId
        return (message, person)

    personId <- requireAuthId
    (widget, enctype) <- generateFormPost (fMessage groupId personId)

    renderWithGroups $(widgetFile "message/list") "グループチャット" groupId


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost (fMessage groupId personId)
    case res of
        FormSuccess message -> do
            _ <- runDB $ insert message
            redirect $ MessageListR groupId
        _ -> do
            redirect $ MessageListR groupId
