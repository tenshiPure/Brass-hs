module Handler.Belong where


import Import


fBelong :: GroupId -> Form Belong
fBelong groupId = renderDivs $ Belong
    <$> areq (selectField sections) "招待する：" Nothing
    <*> areq hiddenField "" (Just groupId)
    where
        sections = do
            belongs <- runDB $ selectList [BelongGroupId ==. groupId] [Asc BelongId]
            let belongedPersonIds = map (belongPersonId . entityVal) belongs
            persons <- runDB $ selectList [PersonId /<-. belongedPersonIds] [Asc PersonId]

            optionsPairs $ map (\p -> (personName $ entityVal p, entityKey p)) persons


getBelongCreateR :: GroupId -> Handler Html
getBelongCreateR groupId = do
    (widget, enctype) <- generateFormPost (fBelong groupId)

    defaultLayout $(widgetFile "belong/create")


postBelongCreateR :: GroupId -> Handler Html
postBelongCreateR groupId = do
    ((res, _), _) <- runFormPost (fBelong groupId)
    case res of
        FormSuccess belong -> do
            _ <- runDB $ insert belong
            redirect $ GroupDetailR groupId
        _ -> do
            setMessage "nothing to do..."
            redirect $ GroupDetailR groupId


getBelongDeleteR :: GroupId -> PersonId -> Handler Html
getBelongDeleteR groupId personId = do
    _ <- runDB $ deleteWhere [BelongPersonId ==. personId, BelongGroupId ==. groupId]
    redirect $ GroupDetailR groupId
