module Handler.Link where


import Import


categories :: [(Text, Text)]
categories = [("音楽", "notes.gif"),
              ("建物", "building.gif"),
              ("宴会", "beer.png"),
              ("書類", "documents.png"),
              ("通知", "megaphone.png"),
              ("動画", "video.png")]


fLink :: GroupId -> PersonId -> Html -> MForm Handler (FormResult Link, Widget)
fLink groupId personId extra = do
    (titleResult, titleView)       <- mreq textField (createSettings "link-form__title" [("placeholder", "タイトルを入力")]) Nothing
    (urlResult, urlView)           <- mreq urlField (createSettings "link-form__url" [("placeholder", "URLを入力")]) Nothing
    (iconResult, iconView)         <- mreq (selectFieldList categories) (createSettings "link-form__category" []) Nothing
    (groupIdResult, groupIdView)   <- mreq hiddenField "" (Just groupId)
    (personIdResult, personIdView) <- mreq hiddenField "" (Just personId)
    let result = Link
           <$> titleResult
           <*> urlResult
           <*> iconResult
           <*> groupIdResult
           <*> personIdResult
        widget = $(widgetFile "link/form/link")
    return (result, widget)


fComment :: LinkId -> PersonId -> Html -> MForm Handler (FormResult Comment, Widget)
fComment linkId personId extra = do
    (bodyResult, bodyView) <- mreq textareaField (createSettings "comment-form__body" [("placeholder", "コメントを入力")]) Nothing
    (linkIdResult, linkIdView) <- mreq hiddenField "" (Just linkId)
    (personIdResult, personIdView) <- mreq hiddenField "" (Just personId)
    let result = Comment
           <$> bodyResult
           <*> linkIdResult
           <*> personIdResult
        widget = $(widgetFile "link/form/comment")
    return (result, widget)


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Asc LinkId]
    contents <- forM links $ \link -> do
        commentNum <- runDB $ count [CommentLinkId ==. entityKey link]
        return (link, commentNum)

    personId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fLink groupId personId

    renderWithGroups $(widgetFile "link/list") "リンク 一覧" PLink ["リンク 一覧"] groupId [$(widgetFile "widget/image-list")]


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    link <- runDB $ get404 linkId
    comments <- fmap (fmap entityVal) $ runDB $ selectList [CommentLinkId ==. linkId] [Asc CommentId]
    contents <- forM comments $ \comment -> do
        person <- runDB $ get404 (commentPersonId comment)
        return (comment, person)

    personId <- requireAuthId
    (formWidget, enctype) <- generateFormPost (fComment linkId personId)

    renderWithGroups $(widgetFile "link/detail") "リンク 詳細" PLink ["リンク 一覧", "リンク 詳細"] groupId [$(widgetFile "widget/media")]


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost $ fLink groupId personId
    case res of
        FormSuccess link -> do
            linkId <- runDB $ insert link

            writeEvent (linkTitle link) groupId personId (Just linkId) Nothing

            redirect $ LinkListR groupId

        _ -> error "todo"


postLinkCommentCreateR :: GroupId -> LinkId -> Handler Html
postLinkCommentCreateR groupId linkId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost (fComment linkId personId)
    case res of
        FormSuccess comment -> do
            commentId <- runDB $ insert comment

            writeEvent "なんらかのコメント" groupId personId (Just linkId) (Just commentId)

            redirect $ LinkDetailR groupId linkId

        _ -> error "todo"


writeEvent :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Text -> GroupId -> PersonId -> Maybe LinkId -> Maybe CommentId -> HandlerT site IO ()
writeEvent body groupId personId mLinkId mCommentId = do
    now <- liftIO getCurrentTime
    _ <- runDB $ insert $ Event body now groupId personId Nothing Nothing Nothing mLinkId mCommentId
    return ()
