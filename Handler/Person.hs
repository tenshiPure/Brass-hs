module Handler.Person where


import Import


getPersonListR :: Handler Html
getPersonListR = do
    persons <- runDB $ selectList [] [Asc PersonId]

    defaultLayout $(widgetFile "person/list")


getPersonDetailR :: PersonId -> Handler Html
getPersonDetailR personId = do
    person <- runDB $ get404 personId

    defaultLayout $(widgetFile "person/detail")
        


fPerson :: Maybe Person -> Form Person
fPerson mPerson = renderDivs $ Person
    <$> areq hiddenField ""     (personMail <$> mPerson)
    <*> areq textField   "名前" (personName <$> mPerson)


getPersonUpdateR :: Handler Html
getPersonUpdateR = do
    personId <- requireAuthId
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ (fPerson $ Just person)

    defaultLayout $(widgetFile "person/update")
        


postPersonUpdateR :: Handler Html
postPersonUpdateR = do
    personId <- requireAuthId
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            runDB $ replace personId person
            redirect $ PersonDetailR personId

        _ -> defaultLayout $(widgetFile "person/update")
