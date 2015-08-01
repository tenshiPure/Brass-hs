module Handler.Message where


import Import
import Data.Time
import Database.Persist.Sql(fromSqlKey)


fMessage :: GroupId -> PersonId -> Html -> MForm Handler (FormResult Message, Widget)
fMessage groupId personId = renderDivs $ Message
    <$> areq textareaField (createSettings "message-form__body" [("placeholder", "内容を入力")]) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq hiddenField "" (Just groupId)
    <*> areq hiddenField "" (Just personId)


getMessageListR :: GroupId -> Handler Html
getMessageListR groupId = do
    authId <- requireAuthId
    (formWidget, enctype) <- generateFormPost $ fMessage groupId authId

    messages <- runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageId]
    contents <- forM messages $ \message -> do
        let personId = messagePersonId $ entityVal message
        person <- runDB $ get404 personId
        return (message, person)

    tz <- liftIO getCurrentTimeZone

    renderWithGroups $(widgetFile "message/list") "チャット" PMessage groupId [$(widgetFile "widget/media")]


postMessageCreateR :: GroupId -> Handler Html
postMessageCreateR groupId = do
    authId <- requireAuthId
    ((res, _), _) <- runFormPost $ fMessage groupId authId
    case res of
        FormSuccess message -> do
            _ <- runDB $ insert message
            redirect $ MessageListR groupId

        _ -> error "todo"
