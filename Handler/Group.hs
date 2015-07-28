module Handler.Group where


import Import


postGroupCreateR :: Handler Html
postGroupCreateR = do
    ((res, _), _) <- runFormPost $ fGroup
    case res of
        FormSuccess entity -> do
            groupId <- runDB $ insert entity

            personId <- requireAuthId
            _ <- runDB $ insert $ Belong groupId personId 

            redirect $ HomeR

        _ -> error "todo"
