module Handler.GroupAdd where


import Import


data Model = Model
    { name :: Text
    } deriving (Show)

form :: Html -> MForm Handler (FormResult Group, Widget)
form extra = do
    (nameResult, nameView) <- mreq textField "" (Just "aaa")
    let result = Group
           <$> nameResult
        widget = $(widgetFile "group/form")
    return (result, widget)


getGroupAddR :: Handler Html
getGroupAddR = do
    (widget, enctype) <- generateFormPost form

    defaultLayout $(widgetFile "group/add")


postGroupAddR :: Handler Html
postGroupAddR = do
    ((res, widget), enctype) <- runFormPost form
    case res of
        FormSuccess group -> do
            groupId <- runDB $ insert group
            setMessage $ toHtml $ (groupName group) <> " created"
            redirect $ GroupDetailR groupId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "group/add")
