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
    ((res, widget), enctype) <- runFormPost (fBelong groupId)
    case res of
        FormSuccess belong -> do
            runDB $ insert belong
            redirect $ PersonListR


postBelongDeleteR :: PersonId -> GroupId -> Handler Html
postBelongDeleteR personId groupId = do
    undefined
