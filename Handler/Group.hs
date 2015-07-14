module Handler.Group where


import Import


getGroupListR :: Handler Html
getGroupListR = do
    entities <- runDB $ selectList [] [Asc GroupId]

    defaultLayout $(widgetFile "group/list")


getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    entity <- runDB $ get404 groupId

    belongs <- runDB $ selectList [BelongGroupId ==. groupId] [Asc BelongPersonId]
    let personIds = map (belongPersonId . entityVal) belongs
    persons <- mapM (runDB . get404) personIds

    let tPersons = zip personIds persons

    defaultLayout $ do
        $(widgetFile "group/detail")


fGroup :: Maybe Group -> Html -> MForm Handler (FormResult Group, Widget)
fGroup mGroup extra = do
    (nameResult, nameView) <- mreq textField "" (groupName <$> mGroup)
    (iconResult, iconView) <- mreq textField "" (groupIcon <$> mGroup)
    let result = Group
           <$> nameResult
           <*> iconResult
        widget = $(widgetFile "group/form")
    return (result, widget)


getGroupCreateR :: Handler Html
getGroupCreateR = do
    (widget, enctype) <- generateFormPost $ fGroup Nothing

    defaultLayout $(widgetFile "group/create")


postGroupCreateR :: Handler Html
postGroupCreateR = do
    ((res, widget), enctype) <- runFormPost $ fGroup Nothing
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
    (widget, enctype) <- generateFormPost $ fGroup (Just entity)

    defaultLayout $(widgetFile "group/update")


postGroupUpdateR :: GroupId -> Handler Html
postGroupUpdateR eId = do
    ((res, widget), enctype) <- runFormPost $ fGroup Nothing
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
