module Handler.Link where


import Import


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Asc LinkId]
    contents <- forM links $ \link -> do
        commentNum <- runDB $ count [CommentLinkId ==. entityKey link]
        return (link, commentNum)

    renderWithGroups $(widgetFile "link/list") "リンク 一覧" PLink groupId


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    link <- runDB $ get404 linkId
    let ownerId = linkPersonId link
    comments <- fmap (fmap entityVal) $ runDB $ selectList [CommentLinkId ==. linkId] [Asc CommentId]
    contents <- forM comments $ \comment -> do
        person <- runDB $ get404 (commentPersonId comment)
        return (comment, person)

    renderWithGroups $(widgetFile "link/detail") "リンク 詳細" PLink groupId


categories :: [(Text, Text)]
categories = [("音楽", "notes.gif"),
              ("建物", "building.gif"),
              ("宴会", "beer.png"),
              ("書類", "documents.png"),
              ("通知", "megaphone.png"),
              ("動画", "video.png")]

fLink :: GroupId -> PersonId -> Form Link
fLink groupId personId = renderDivs $ Link
    <$> areq textField                    "タイトル" Nothing
    <*> areq urlField                     "リンク先" Nothing
    <*> areq (selectFieldList categories) "ジャンル" Nothing
    <*> areq hiddenField                  ""         (Just groupId)
    <*> areq hiddenField                  ""         (Just personId)


getLinkCreateR :: GroupId -> Handler Html
getLinkCreateR groupId = do
    personId <- requireAuthId
    (widget, enctype) <- generateFormPost (fLink groupId personId)

    defaultLayout $(widgetFile "link/create")


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    personId <- requireAuthId
    ((res, widget), enctype) <- runFormPost (fLink groupId personId)
    case res of
        FormSuccess link -> do
            linkId <- runDB $ insert link
            redirect $ LinkDetailR groupId linkId

        _ -> defaultLayout $(widgetFile "link/create")
