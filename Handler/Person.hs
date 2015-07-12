module Handler.Person where


import Import


getPersonListR :: Handler Html
getPersonListR = do
    persons <- runDB $ selectList [] [Asc PersonId]

    defaultLayout $(widgetFile "person/list")


getPersonDetailR :: PersonId -> Handler Html
getPersonDetailR personId = do
    person <- runDB $ get404 personId

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/detail")


fPerson :: Maybe Person -> Form Person
fPerson mPerson = renderDivs $ Person
    <$> areq hiddenField ""     (personMail <$> mPerson)
    <*> areq textField   "名前" (personName <$> mPerson)


getPersonCreateR :: Handler Html
getPersonCreateR = do
    (widget, enctype) <- generateFormPost (fPerson Nothing)

    defaultLayout $(widgetFile "person/create")


postPersonCreateR :: Handler Html
postPersonCreateR = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            personId <- runDB $ insert person
            setMessage $ toHtml $ (personName person) <> " created"
            redirect $ PersonDetailR personId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/create")


getPersonUpdateR :: PersonId -> Handler Html
getPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ (fPerson $ Just person)

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/update")


postPersonUpdateR :: PersonId -> Handler Html
postPersonUpdateR personId = do
    ((res, widget), enctype) <- runFormPost (fPerson Nothing)
    case res of
        FormSuccess person -> do
            runDB $ replace personId person
            setMessage $ toHtml $ (personName person) <> " updated"
            redirect $ PersonDetailR personId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/update")


postPersonDeleteR :: PersonId -> Handler Html
postPersonDeleteR personId = do
    runDB $ delete personId
    redirect $ PersonListR
