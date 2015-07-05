module Handler.Group where


import Import


getGroupListR :: Handler Html
getGroupListR = do
    groups <- runDB $ selectList [] [Desc GroupId]

    defaultLayout $(widgetFile "group/list")


getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    group <- runDB $ get404 groupId

    defaultLayout $ do
        setTitle $ toHtml $ groupName group
        $(widgetFile "group/detail")


form :: Maybe Group -> Html -> MForm Handler (FormResult Group, Widget)
form mGroup extra = do
    (nameResult, nameView) <- mreq textField "" (groupName <$> mGroup)
    let result = Group
           <$> nameResult
        widget = $(widgetFile "group/form")
    return (result, widget)


getGroupCreateR :: Handler Html
getGroupCreateR = do
    (widget, enctype) <- generateFormPost $ form Nothing

    defaultLayout $(widgetFile "group/create")


postGroupCreateR :: Handler Html
postGroupCreateR = do
    ((res, widget), enctype) <- runFormPost $ form Nothing
    case res of
        FormSuccess group -> do
            groupId <- runDB $ insert group
            setMessage $ toHtml $ (groupName group) <> " created"
            redirect $ GroupDetailR groupId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "group/create")


getGroupUpdateR :: GroupId -> Handler Html
getGroupUpdateR groupId = do
    group <- runDB $ get404 groupId
    (widget, enctype) <- generateFormPost $ form (Just group)

    defaultLayout $(widgetFile "group/update")


postGroupUpdateR :: GroupId -> Handler Html
postGroupUpdateR groupId = do
    ((res, widget), enctype) <- runFormPost $ form Nothing
    case res of
        FormSuccess group -> do
            runDB $ replace groupId group
            setMessage $ toHtml $ (groupName group) <> " update"
            redirect $ GroupDetailR groupId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "group/update")


postGroupDeleteR :: GroupId -> Handler Html
postGroupDeleteR groupId = do
    runDB $ delete groupId
    redirect $ GroupListR
