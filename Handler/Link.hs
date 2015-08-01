module Handler.Link where


import Import
import Database.Persist.Sql(fromSqlKey)


categories :: [(Text, Text)]
categories = [("音楽", "notes.gif"),
              ("建物", "building.gif"),
              ("宴会", "beer.png"),
              ("書類", "documents.png"),
              ("通知", "megaphone.png"),
              ("動画", "video.png")]


fLink :: GroupId -> PersonId -> Html -> MForm Handler (FormResult Link, Widget)
fLink groupId personId = renderDivs $ Link
    <$> areq textField                    (createSettings "link-form__title"    [("placeholder", "タイトルを入力")]) Nothing
    <*> areq urlField                     (createSettings "link-form__url"      [("placeholder", "URLを入力")])      Nothing
    <*> areq (selectFieldList categories) (createSettings "link-form__category" [])                                  Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq hiddenField "" (Just groupId)
    <*> areq hiddenField "" (Just personId)


fComment :: LinkId -> GroupId -> PersonId -> Html -> MForm Handler (FormResult Comment, Widget)
fComment linkId groupId personId = renderDivs $ Comment
    <$> areq textareaField (createSettings "comment-form__body" [("placeholder", "コメントを入力")]) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq hiddenField "" (Just linkId)
    <*> areq hiddenField "" (Just groupId)
    <*> areq hiddenField "" (Just personId)


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [LinkGroupId ==. groupId] [Desc LinkId]
    contents <- forM links $ \link -> do
        commentNum <- runDB $ count [CommentLinkId ==. entityKey link]
        return (link, commentNum)

    personId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fLink groupId personId

    renderWithGroups $(widgetFile "link/list") "リンク 一覧" PLink groupId [$(widgetFile "widget/image-list")]


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    link <- runDB $ get404 linkId
    comments <- runDB $ selectList [CommentLinkId ==. linkId] [Desc CommentId]
    contents <- forM comments $ \comment -> do
        person <- runDB $ get404 (commentPersonId $ entityVal comment)
        return (comment, person)

    personId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fComment linkId groupId personId

    renderWithGroups $(widgetFile "link/detail") "リンク 詳細" PLink groupId [$(widgetFile "widget/media")]


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost $ fLink groupId personId
    case res of
        FormSuccess link -> do
            _ <- runDB $ insert link
            redirect $ LinkListR groupId

        _ -> error "todo"


postLinkCommentCreateR :: GroupId -> LinkId -> Handler Html
postLinkCommentCreateR groupId linkId = do
    personId <- requireAuthId
    ((res, _), _) <- runFormPost $ fComment linkId groupId personId
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            redirect $ LinkDetailR groupId linkId

        _ -> error "todo"
