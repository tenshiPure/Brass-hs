module Handler.Belong where


import Import


fBelong :: GroupId -> Form Belong
fBelong groupId = renderDivs $ Belong
    <$> areq (selectField sections) "招待する：" Nothing
    <*> areq hiddenField "" (Just groupId)
    where
        sections = do
            persons <- runDB $ selectList [] [Asc PersonId]
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


getBelongDeleteR :: PersonId -> GroupId -> Handler Html
getBelongDeleteR personId groupId = do
    _ <- runDB $ deleteWhere [BelongPersonId ==. personId, BelongGroupId ==. groupId]
    redirect $ GroupDetailR groupId
