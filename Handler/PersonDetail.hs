module Handler.PersonDetail where


import Import


getPersonDetailR :: PersonId -> Handler Html
getPersonDetailR personId = do
    person <- runDB $ get404 personId
    group  <- runDB $ get404 (personGroupId person)

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/detail")
