module Handler.Sample where


import Import


form :: Form Sample
form = renderDivs $ Sample
    <$> areq textField "名前" Nothing


getSampleR :: Handler Html
getSampleR = do
    (widget, enctype) <- generateFormPost form

    defaultLayout $(widgetFile "sample")


postSampleR :: Handler Html
postSampleR = do
    verb <- lookupPostParam "verb"
    case verb of
        Just "create" -> defaultLayout [whamlet|<p>create!|]
        Just "update" -> defaultLayout [whamlet|<p>update!|]
        Just "delete" -> defaultLayout [whamlet|<p>delete!|]
        _             -> defaultLayout [whamlet|<p>error!|]


getSampleStringR :: PersonId -> Handler String
getSampleStringR personId = do
    person <- runDB $ get404 personId
    return $ show person


getSampleAuthR :: Handler String
getSampleAuthR = do
    person <- requireAuth
    return $ show person
