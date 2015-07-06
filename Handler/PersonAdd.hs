module Handler.PersonAdd where


import Import


form :: Form Person
form = renderDivs $ Person
    <$> areq textField "名前" Nothing


getPersonAddR :: Handler Html
getPersonAddR = do
    (widget, enctype) <- generateFormPost form

    defaultLayout $(widgetFile "person/add")


postPersonAddR :: Handler Html
postPersonAddR = do
    ((res, widget), enctype) <- runFormPost form
    case res of
        FormSuccess person -> do
            personId <- runDB $ insert person
            setMessage $ toHtml $ (personName person) <> " created"
            redirect $ PersonDetailR personId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/add")
