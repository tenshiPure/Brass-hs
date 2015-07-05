module Handler.PersonUpdate where


import Import


form :: Maybe Person -> Form Person
form mPerson = renderDivs $ Person
    <$> areq textField "名前" (personName <$> mPerson)
    <*> areq textField "楽器" (personInstrument <$> mPerson)
    <*> areq (selectField sections) "グループ" (personGroupId <$> mPerson)
    where
        sections = do
            entities <- runDB $ selectList [] [Asc GroupName]
            optionsPairs $ map (\e -> (groupName $ entityVal e, entityKey e)) entities


getPersonUpdateR :: PersonId -> Handler Html
getPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (widget, enctype) <- generateFormPost $ form (Just person)

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/update")


postPersonUpdateR :: PersonId -> Handler Html
postPersonUpdateR personId = do
    ((res, widget), enctype) <- runFormPost (form Nothing)
    case res of
        FormSuccess person -> do
            runDB $ replace personId person
            setMessage $ toHtml $ (personName person) <> " updated"
            redirect $ PersonDetailR personId

        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/update")
