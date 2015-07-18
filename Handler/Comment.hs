module Handler.Comment where


import Import


fComment :: LinkId -> PersonId -> Form Comment
fComment linkId personId = renderDivs $ Comment
    <$> areq textField   "コメント" Nothing
    <*> areq hiddenField ""         (Just linkId)
    <*> areq hiddenField ""         (Just personId)


getCommentCreateR :: GroupId -> LinkId -> Handler Html
getCommentCreateR groupId linkId = do
    personId <- requireAuthId
    (widget, enctype) <- generateFormPost (fComment linkId personId)

    defaultLayout $(widgetFile "comment/create")


postCommentCreateR :: GroupId -> LinkId -> Handler Html
postCommentCreateR groupId linkId = do
    personId <- requireAuthId
    ((res, widget), enctype) <- runFormPost (fComment linkId personId)
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            redirect $ LinkDetailR groupId linkId

        _ -> defaultLayout $(widgetFile "comment/create")
