module Handler.Link where


import Import


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Asc LinkId]
    contents <- forM links $ \link -> do
        count <- runDB $ count [CommentLinkId ==. entityKey link]
        return (entityVal link, count)

    renderWithGroups $(widgetFile "link/list") "リンク" PLink groupId


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    entity <- runDB $ get404 groupId
    link <- runDB $ get404 linkId
    comments <- runDB $ selectList [CommentLinkId ==. linkId] [Asc CommentId]

    defaultLayout $(widgetFile "link/detail")


fLink :: GroupId -> Maybe Link -> Form Link
fLink groupId mLink = renderDivs $ Link
    <$> areq textField   "タイトル" (linkTitle <$> mLink)
    <*> areq urlField    "リンク先" (linkUrl   <$> mLink)
    <*> areq hiddenField ""         (Just groupId)


getLinkCreateR :: GroupId -> Handler Html
getLinkCreateR groupId = do
    (widget, enctype) <- generateFormPost (fLink groupId Nothing)

    defaultLayout $(widgetFile "link/create")


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    ((res, widget), enctype) <- runFormPost (fLink groupId Nothing)
    case res of
        FormSuccess link -> do
            linkId <- runDB $ insert link
            redirect $ LinkDetailR groupId linkId

        _ -> defaultLayout $(widgetFile "link/create")
