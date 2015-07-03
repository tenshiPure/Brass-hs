module Handler.PersonAdd where


import Import


form :: Form Person
form = renderDivs $ Person
    <$> areq textField "名前" Nothing
    <*> areq textField "楽器" Nothing


getPersonAddR :: Handler Html
getPersonAddR = do
    (widget, enctype) <- generateFormPost form
    let links = $(widgetFile "person/links")

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
            let links = $(widgetFile "person/links")
            setTitle "Invalid Input"
            $(widgetFile "person/add")
