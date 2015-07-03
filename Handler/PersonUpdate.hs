module Handler.PersonUpdate where

import Import

form :: Maybe Person -> Form Person
form (Just person) = renderDivs $ Person
    <$> areq textField "名前" (Just $ personName person)
    <*> areq textField "楽器" (Just $ personInstrument person)
form Nothing = renderDivs $ Person
    <$> areq textField "名前" Nothing
    <*> areq textField "楽器" Nothing

getPersonUpdateR :: PersonId -> Handler Html
getPersonUpdateR personId = do
    person <- runDB $ get404 personId
    (personWidget, enctype) <- generateFormPost $ form (Just person)

    defaultLayout $ do
        setTitle $ toHtml $ personName person
        $(widgetFile "person/update")

postPersonUpdateR :: PersonId -> Handler Html
postPersonUpdateR personId = do
    ((res, personWidget), enctype) <- runFormPost (form Nothing)
    case res of
        FormSuccess person -> do
            runDB $ updateWhere [PersonId ==. personId] [PersonName =. (personName person), PersonInstrument =. (personInstrument person)]
            setMessage $ toHtml $ (personName person) <> " updated"
            redirect $ PersonDetailR personId
        _ -> defaultLayout $ do
            setTitle "Invalid Input"
            $(widgetFile "person/update")
