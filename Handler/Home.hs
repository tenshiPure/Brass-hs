module Handler.Home where


import Import
import Database.Persist.Sql(fromSqlKey)

getHomeR :: Handler Html
getHomeR = do
    personId <- requireAuthId

    belongs <- runDB $ selectList [BelongPersonId ==. personId] [Asc BelongId]
    let belongGroupIds = map (belongGroupId . entityVal) belongs
    mGroup <- runDB $ selectFirst [GroupId <-. belongGroupIds] [Asc GroupId]

    case mGroup of
        (Just topGroup) -> redirect $ HomeWithGroupIdR (entityKey topGroup)
        Nothing      -> error "no join group"


getHomeWithGroupIdR :: GroupId -> Handler Html
getHomeWithGroupIdR groupId = do
    messages <- runDB $ selectList [MessageGroupId ==. groupId] [Desc MessageId]


    now <- liftIO getCurrentTime

    let eventLogs = map (toEventLog now) messages

    defaultLayout [whamlet|
        $forall eventLog <- eventLogs
            $case eventLog
                $of MessageEventLog groupId messageId body _
                    <p>
                        チャットで
                        <a href=@{MessageListR groupId}#message-#{messageId}>
                            #{body}
                        と発言しました
    |]

--     tz <- liftIO getCurrentTimeZone

--     renderWithGroups $(widgetFile "home/home") "ホーム" PHome groupId [$(widgetFile "widget/media")]


data EventLog = MessageEventLog { groupId :: GroupId, messageId :: Int64, body :: Text, now :: UTCTime }
--            | LinkEventLog { linkId :: String, title :: String , now :: Int, groupId :: Int}
--            | CommentEventLog { linkId :: String, commentId :: String, linkTitle :: String, body :: String , now :: Int, groupId :: Int}
           deriving (Show)


toEventLog :: UTCTime -> Entity Message -> EventLog
toEventLog now message = MessageEventLog (messageGroupId $ entityVal message) (fromSqlKey $ entityKey message) (messageBody $ entityVal message) now
--         (MessageEvent id body _ _) -> putStrLn $ "チャットで<a href=/messages/" ++ id ++ ">" ++ body ++ "と発言しました"
--         (LinkEvent id title _ _) -> putStrLn $ "リンク<a href=/links/" ++ id ++ ">" ++ title ++ "を作成しました"
--         (CommentEvent lid cid title body _ _) -> putStrLn $ "リンク<a href=/links/" ++ lid ++ ">" ++ title ++ "に<a href=/comments/" ++ cid ++ ">" ++ body ++ "とコメントしました"
