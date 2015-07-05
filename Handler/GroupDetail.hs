module Handler.GroupDetail where

import Import

getGroupDetailR :: GroupId -> Handler Html
getGroupDetailR groupId = do
    group <- runDB $ get404 groupId

    defaultLayout $ do
        setTitle $ toHtml $ groupName group
        $(widgetFile "group/detail")
