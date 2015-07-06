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


fPerson :: Html -> MForm Handler (FormResult (Person, Maybe (Key Instrument)), Widget)
fPerson = renderDivs $ (,)
    <$> (
        Person <$> areq textField "名前" Nothing
    )
    <*> aopt (selectField sections) "その他" Nothing
    where
        sections = do
            entities <- runDB $ selectList [] [Asc InstrumentId]
            optionsPairs $ map (\e -> (instrumentName $ entityVal e, entityKey e)) entities


getPersonAddR :: Handler Html
getPersonAddR = do
    (widget, enctype) <- generateFormPost fPerson

    defaultLayout $(widgetFile "person/add")


postPersonAddR :: Handler Html
postPersonAddR = do
    ((res, widget), enctype) <- runFormPost fPerson
    case res of
        FormSuccess (person, _) -> do
            personId <- runDB $ insert person
            setMessage $ toHtml $ (personName person) <> " created"
            redirect $ PersonDetailR personId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/add")


getPersonUpdateR :: PersonId -> Handler Html
getPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ fPerson

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/update")


postPersonUpdateR :: PersonId -> Handler Html
postPersonUpdateR personId = do
    ((res, widget), enctype) <- runFormPost fPerson
    case res of
        FormSuccess (person, _) -> do
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
