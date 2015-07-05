module Handler.PersonDetail where


import Import


getPersonDetailR :: PersonId -> Handler Html
getPersonDetailR personId = do
    person <- runDB $ get404 personId

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/detail")
