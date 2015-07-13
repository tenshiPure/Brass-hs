module Handler.Link where


import Import


getLinkListR :: GroupId -> Handler Html
getLinkListR groupId = do
    links <- runDB $ selectList [] [Asc LinkId]

    defaultLayout $(widgetFile "link/list")


fLink :: Maybe Link -> Form Link
fLink mLink = renderDivs $ Link
    <$> areq urlField "リンク先" (linkContent <$> mLink)


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
