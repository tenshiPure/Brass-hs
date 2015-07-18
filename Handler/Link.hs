module Handler.Link where


import Import


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Asc LinkId]
    contents <- forM links $ \link -> do
        count <- runDB $ count [CommentLinkId ==. entityKey link]
        return (link, count)

    renderWithGroups $(widgetFile "link/list") "リンク" PLink groupId


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    entity <- runDB $ get404 groupId
    link <- runDB $ get404 linkId
    comments <- runDB $ selectList [CommentLinkId ==. linkId] [Asc CommentId]

    defaultLayout $(widgetFile "link/detail")


categories :: [(Text, Text)]
categories = [("音楽", "notes.gif"),
              ("建物", "building.gif"),
              ("宴会", "beer.png"),
              ("書類", "documents.png"),
              ("通知", "megaphone.png"),
              ("動画", "video.png")]

fLink :: GroupId -> Form Link
fLink groupId = renderDivs $ Link
    <$> areq textField                    "タイトル" Nothing
    <*> areq urlField                     "リンク先" Nothing
    <*> areq (selectFieldList categories) "ジャンル" Nothing
    <*> areq hiddenField                  ""         (Just groupId)


getLinkCreateR :: GroupId -> Handler Html
getLinkCreateR groupId = do
    (widget, enctype) <- generateFormPost (fLink groupId)

    defaultLayout $(widgetFile "link/create")


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    ((res, widget), enctype) <- runFormPost (fLink groupId)
    case res of
        FormSuccess link -> do
            linkId <- runDB $ insert link
            redirect $ LinkDetailR groupId linkId

        _ -> defaultLayout $(widgetFile "link/create")
