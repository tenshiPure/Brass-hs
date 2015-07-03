module Handler.Person (
getPersonR,
postPersonR,
updatePersonR,
deletePersonR
) where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

form :: Form Person
form = renderDivs $ Person
    <$> areq textField "名前" Nothing
    <*> areq textField "楽器" Nothing

getPersonR :: PersonId -> Handler Html
getPersonR personId = do
    person <- runDB $ get404 personId

    (personWidget, enctype) <- generateFormPost form
    defaultLayout $ do
        $(widgetFile "person")

postPersonR :: PersonId -> Handler Html
postPersonR personId = do
    ((res, personWidget), enctype) <- runFormPost form
    case res of
        FormSuccess person -> do
            personId <- runDB $ insert person
            setMessage $ toHtml $ (personName person) <> "　を作成しました"
            redirect $ PersonR personId

updatePersonR :: PersonId -> Handler Html
updatePersonR = error "Not yet implemented: updatePersonR"

deletePersonR :: PersonId -> Handler Html
deletePersonR = error "Not yet implemented: deletePersonR"
