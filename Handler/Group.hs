module Handler.Group where


import Import


postGroupCreateR :: GroupId -> Handler Html
postGroupCreateR currentGroupId = do
    ((res, _), _) <- runFormPost $ fGroup Nothing
    case res of
        FormSuccess entity -> do
            mGroupId <- runDB $ insertUnique entity

            case mGroupId of
                (Just groupId) -> do
                    authId <- requireAuthId
                    createBelong groupId authId

                    now <- liftIO getCurrentTime
                    _ <- runDB $ insert $ GroupLog (groupName entity) (groupIcon entity) now groupId authId

                    setSuccessInformation $ (groupName entity) ++ " を作成しました"

                    redirect $ HomeWithGroupIdR groupId

                Nothing -> do
                    setWarningInformation $ (groupName entity) ++ " は同名のグループが既に存在するため作成できませんでした"

                    redirect $ HomeR

        _ -> do
            setErrorInformation "不正な入力値のため作成できませんでした"

            redirect $ HomeWithGroupIdR currentGroupId


postGroupUpdateR :: GroupId -> Handler Html
postGroupUpdateR currentGroupId = do
    ((res, _), _) <- runFormPost $ fGroup Nothing
    case res of
        FormSuccess entity -> do
            mEntity <- runDB $ checkUnique entity

            now <- liftIO getCurrentTime
            authId <- requireAuthId

            case mEntity of
                Nothing -> do
                    _ <- runDB $ replace currentGroupId entity

                    _ <- runDB $ insert $ GroupLog (groupName entity) (groupIcon entity) now currentGroupId authId

                    setSuccessInformation $ (groupName entity) ++ " を更新しました"

                    redirect $ HomeWithGroupIdR currentGroupId

                (Just _) -> do
                    currentGroup <- runDB $ get404 currentGroupId

                    case (groupName currentGroup) == (groupName entity) of
                        True -> do
                            _ <- runDB $ replace currentGroupId entity

                            _ <- runDB $ insert $ GroupLog (groupName entity) (groupIcon entity) now currentGroupId authId

                            setSuccessInformation $ (groupName entity) ++ " を更新しました"

                            redirect $ HomeWithGroupIdR currentGroupId

                        False -> do
                            setWarningInformation $ (groupName entity) ++ " は同名の別グループが既に存在するため更新できませんでした"

                            redirect $ HomeWithGroupIdR currentGroupId

        _ -> do
            setErrorInformation "不正な入力値のため作成できませんでした"

            redirect $ HomeWithGroupIdR currentGroupId


getGroupInvitedR :: GroupId -> Handler Html
getGroupInvitedR groupId = do
    authId <- requireAuthId

    group' <- runDB $ get404 groupId
    mBelong <- runDB $ selectFirst [BelongGroupId ==. groupId, BelongPersonId ==. authId] [Asc BelongId]

    case mBelong of
        (Just _) -> do
            setWarningInformation $ (groupName group') ++ " には参加済みです"

            redirect $ HomeWithGroupIdR groupId

        Nothing  -> do
            createBelong groupId authId

            setSuccessInformation $ (groupName group') ++ " に参加しました"

            redirect $ HomeWithGroupIdR groupId


getBelongDeleteR :: GroupId -> Handler Html
getBelongDeleteR groupId = do
    authId <- requireAuthId
    deleteBelong groupId authId

    group' <- runDB $ get404 groupId

    setSuccessInformation $ (groupName group') ++ " から退席しました"

    redirect $ HomeR


getGroupDeleteR :: GroupId -> Handler Html
getGroupDeleteR groupId = do
    group' <- runDB $ get404 groupId

    _ <- runDB $ deleteWhere [GroupId ==. groupId]
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId]

    setSuccessInformation $ (groupName group') ++ " を削除しました"

    redirect $ HomeR
