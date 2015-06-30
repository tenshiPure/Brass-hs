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
    persons <- runDB $ selectList [] [Desc PersonId]

    (personWidget, enctype) <- generateFormPost form
    defaultLayout $ do
        $(widgetFile "person")

postPersonR :: PersonId -> Handler Html
postPersonR = error "Not yet implemented: postPersonR"

updatePersonR :: PersonId -> Handler Html
updatePersonR = error "Not yet implemented: updatePersonR"

deletePersonR :: PersonId -> Handler Html
deletePersonR = error "Not yet implemented: deletePersonR"
