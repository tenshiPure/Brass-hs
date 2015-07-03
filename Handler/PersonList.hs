module Handler.PersonList (
getPersonListR
) where


import Import


getPersonListR :: Handler Html
getPersonListR = do
    persons <- runDB $ selectList [] [Desc PersonId]
    let links = $(widgetFile "person/links")

    defaultLayout $(widgetFile "person/list")
