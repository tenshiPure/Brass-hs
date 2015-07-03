module Handler.PersonDelete where

import Import

postPersonDeleteR :: PersonId -> Handler Html
postPersonDeleteR personId = do
    runDB $ delete personId
    redirect $ PersonListR
