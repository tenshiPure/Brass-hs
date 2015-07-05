module Handler.Group where


import Import


getGroupListR :: Handler Html
getGroupListR = do
    entities <- runDB $ selectList [] [Desc GroupId]

    defaultLayout $(widgetFile "group/list")


getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR eId = do
    entity <- runDB $ get404 eId

    defaultLayout $ do
        setTitle $ toHtml $ groupName entity
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
        FormSuccess entity -> do
            eId <- runDB $ insert entity
            setMessage $ toHtml $ (groupName entity) <> " created"
            redirect $ GroupDetailR eId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "group/create")


getGroupUpdateR :: GroupId -> Handler Html
getGroupUpdateR eId = do
    entity <- runDB $ get404 eId
    (widget, enctype) <- generateFormPost $ form (Just entity)

    defaultLayout $(widgetFile "group/update")


postGroupUpdateR :: GroupId -> Handler Html
postGroupUpdateR eId = do
    ((res, widget), enctype) <- runFormPost $ form Nothing
    case res of
        FormSuccess entity -> do
            runDB $ replace eId entity
            setMessage $ toHtml $ (groupName entity) <> " update"
            redirect $ GroupDetailR eId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "group/update")


postGroupDeleteR :: GroupId -> Handler Html
postGroupDeleteR eId = do
    runDB $ delete eId
    redirect $ GroupListR
