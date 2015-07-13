module Handler.Link where


import Import


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [] [Asc LinkId]

    defaultLayout $(widgetFile "link/list")


getLinkDetailR :: GroupId -> LinkId -> Handler Html
getLinkDetailR groupId linkId = do
    entity <- runDB $ get404 groupId
    link <- runDB $ get404 linkId
    let comments = ["hoge", "fuga"]

    defaultLayout $(widgetFile "link/detail")


fLink :: Maybe Link -> Form Link
fLink mLink = renderDivs $ Link
    <$> areq textField "タイトル" (linkTitle <$> mLink)
    <*> areq urlField  "リンク先" (linkUrl   <$> mLink)


getLinkCreateR :: GroupId -> Handler Html
getLinkCreateR groupId = do
    (widget, enctype) <- generateFormPost (fLink Nothing)

    defaultLayout $(widgetFile "link/create")


postLinkCreateR :: GroupId -> Handler Html
postLinkCreateR groupId = do
    ((res, widget), enctype) <- runFormPost (fLink Nothing)
    case res of
        FormSuccess link -> do
            personId <- runDB $ insert link
            redirect $ LinkListR groupId

        _ -> defaultLayout $(widgetFile "link/create")
