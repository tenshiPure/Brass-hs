module Handler.PersonList (
getPersonListR
) where

import Import

-- form :: Form Person
-- form = renderDivs $ Person
--     <$> areq textField "名前" Nothing
--     <*> areq textField "楽器" Nothing

getPersonListR :: Handler Html
getPersonListR = do
    persons <- runDB $ selectList [] [Desc PersonId]

    defaultLayout $(widgetFile "persons")
