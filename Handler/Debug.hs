module Handler.Debug where


import Import


fPerson :: Maybe Person -> Form Person
fPerson mPerson = renderDivs $ Person
    <$> areq textField "メール" (personMail <$> mPerson)
    <*> areq textField "名前"   (personName <$> mPerson)


getDebugPersonCreateR :: Handler Html
getDebugPersonCreateR = do
    (widget, enctype) <- generateFormPost (fPerson Nothing)

    defaultLayout $(widgetFile "debug/person/create")


postDebugPersonCreateR :: Handler Html
postDebugPersonCreateR = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            personId <- runDB $ insert person
            redirect $ PersonDetailR personId

        _ -> defaultLayout $(widgetFile "debug/person/create")
            


getDebugPersonUpdateR :: PersonId -> Handler Html
getDebugPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ (fPerson $ Just person)

    defaultLayout $(widgetFile "person/update")
        


postDebugPersonUpdateR :: PersonId -> Handler Html
postDebugPersonUpdateR personId = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            runDB $ replace personId person
            redirect $ PersonDetailR personId

        _ -> defaultLayout $(widgetFile "person/update")
            


postDebugPersonDeleteR :: PersonId -> Handler Html
postDebugPersonDeleteR personId = do
    runDB $ delete personId
    redirect $ PersonListR
