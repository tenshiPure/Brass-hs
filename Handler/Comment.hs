module Handler.Comment where


import Import


fComment :: LinkId -> Maybe Comment -> Form Comment
fComment linkId mComment = renderDivs $ Comment
    <$> areq textField   "コメント" (commentBody <$> mComment)
    <*> areq hiddenField ""         (Just linkId)


getCommentCreateR :: GroupId -> LinkId -> Handler Html
getCommentCreateR groupId linkId = do
    (widget, enctype) <- generateFormPost (fComment linkId Nothing)

    defaultLayout $(widgetFile "comment/create")


postCommentCreateR :: GroupId -> LinkId -> Handler Html
postCommentCreateR groupId linkId = do
    ((res, widget), enctype) <- runFormPost (fComment linkId Nothing)
    case res of
        FormSuccess comment -> do
            commentId <- runDB $ insert comment
            redirect $ LinkDetailR groupId linkId

        _ -> defaultLayout $(widgetFile "comment/create")
