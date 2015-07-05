module Handler.PersonList (
getPersonListR
) where


import Import


getPersonListR :: Handler Html
getPersonListR = do
    persons <- runDB $ selectList [] [Desc PersonId]

    defaultLayout $(widgetFile "person/list")
