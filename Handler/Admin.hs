module Handler.Admin where


import Import
import Database.Persist.Sql(toSqlKey, fromSqlKey)


getAdminLinksR :: Handler Html
getAdminLinksR = do
    persons <- runDB $ selectList [] [Asc PersonId]
    defaultLayout $(widgetFile "admin/links")


getAdminInitR :: Handler Html
getAdminInitR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Group])
    _ <- runDB $ insert $ Group "東京シティブラスオルケスター"     "tcbo.jpg"
    _ <- runDB $ insert $ Group "江戸川管楽合奏団"                 "default_1.jpg"
    _ <- runDB $ insert $ Group "高輪台高等学校吹奏楽部"           "tokai.jpg"
    _ <- runDB $ insert $ Group "新交響楽団"                       "default_1.jpg"
    _ <- runDB $ insert $ Group "東京隆生吹奏楽団"                 "default_1.jpg"
    _ <- runDB $ insert $ Group "マシュアールウィンドオーケストラ" "default_1.jpg"
    _ <- runDB $ insert $ Group "伊藤マリーンズ"                   "marines.jpg"
    _ <- runDB $ insert $ Group "東京スタックアート吹奏楽団"       "default_1.jpg"
    _ <- runDB $ insert $ Group "創価グロリア吹奏楽団"             "default_1.jpg"
    _ <- runDB $ insert $ Group "ミュゼダール吹奏楽団"             "default_1.jpg"
    _ <- runDB $ insert $ Group "土気シビックウィンドオーケストラ" "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 01"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 02"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 03"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 04"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 05"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 06"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 07"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 08"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 09"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 10"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 11"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 12"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 13"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 14"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 15"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 16"              "default_1.jpg"
    _ <- runDB $ insert $ Group "ダミーグループ - 17"              "default_1.jpg"

    _ <- runDB $ deleteWhere ([] :: [Filter Person])
    _ <- runDB $ insert $ Person "s-r.com" "ほげ" "hoge.png"
    _ <- runDB $ insert $ Person "m-i.com" "松本" "default_1.png"
    _ <- runDB $ insert $ Person "t-h.com" "千葉" "default_2.png"
    _ <- runDB $ insert $ Person "i-k.com" "伊藤" "default_1.png"
    _ <- runDB $ insert $ Person "m-y.com" "宮本" "default_2.png"
    _ <- runDB $ insert $ Person "o-j.com" "大瀧" "default_1.png"
    _ <- runDB $ insert $ Person "o-a.com" "大渕" "default_1.png"
    _ <- runDB $ insert $ Person "t-t.com" "田中" "default_1.png"

    _ <- runDB $ deleteWhere ([] :: [Filter Belong])
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  3 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  7 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 12 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 13 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 14 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 15 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 16 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 17 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 18 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 19 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 20 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 21 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 22 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 23 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 24 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 25 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 26 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 27 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey 28 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  4 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  2 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  2 :: GroupId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  7 :: GroupId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  2 :: GroupId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  7 :: GroupId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  1 :: GroupId) (toSqlKey 6 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  3 :: GroupId) (toSqlKey 6 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  3 :: GroupId) (toSqlKey 7 :: PersonId)
    _ <- runDB $ insert $ Belong (toSqlKey  5 :: GroupId) (toSqlKey 7 :: PersonId)

    now <- liftIO getCurrentTime
    let longBody = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    let shortBody = "Lorem ipsum dolor sit amet"
    _ <- runDB $ deleteWhere ([] :: [Filter Message])
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 1 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Message longBody  now (toSqlKey 3 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 7 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Message shortBody now (toSqlKey 7 :: GroupId) (toSqlKey 4 :: PersonId)

    _ <- runDB $ deleteWhere ([] :: [Filter Schedule])
    _ <- runDB $ insert $ Schedule "20150705" (Just "深川二中") (Just "中島先生レッスン")                                      (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Schedule "20150705" (Just "青少年センター") Nothing                                                  (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Schedule "20150705" Nothing                 (Just "場所未定")                                        (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Schedule "20150705" (Just "青少年センター") (Just "午前中は分奏、午後はウエストサイドと幻想4,5です") (toSqlKey 1 :: GroupId)

    _ <- runDB $ deleteWhere ([] :: [Filter Attendance])
    _ <- runDB $ insert $ Attendance 1 Nothing                 (toSqlKey 1 :: PersonId) (toSqlKey 1 :: ScheduleId) 
    _ <- runDB $ insert $ Attendance 4 (Just "15時頃帰ります") (toSqlKey 2 :: PersonId) (toSqlKey 1 :: ScheduleId) 

    let url = "https://www.google.co.jp/"
    _ <- runDB $ deleteWhere ([] :: [Filter Link])
    _ <- runDB $ insert $ Link "東ブラのブログ"   url "notes.gif"     (toSqlKey 1 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Link "ToySparkの演奏会" url "notes.gif"     (toSqlKey 7 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Link "深川二中の場所"   url "building.gif"  (toSqlKey 7 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Link "打ち上げ（7/20）" url "beer.png"      (toSqlKey 7 :: GroupId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Link "2015総会資料"     url "documents.png" (toSqlKey 7 :: GroupId) (toSqlKey 2 :: PersonId)
    _ <- runDB $ insert $ Link "衣装について"     url "megaphone.png" (toSqlKey 7 :: GroupId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Link "録音（7/20）"     url "video.png"     (toSqlKey 7 :: GroupId) (toSqlKey 4 :: PersonId)

    _ <- runDB $ deleteWhere ([] :: [Filter Comment])
    _ <- runDB $ insert $ Comment (Textarea longBody)         (toSqlKey 2 :: LinkId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea longBody)         (toSqlKey 2 :: LinkId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea shortBody)        (toSqlKey 2 :: LinkId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea longBody)         (toSqlKey 2 :: LinkId) (toSqlKey 3 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea "アップしました") (toSqlKey 7 :: LinkId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea "あざますー")     (toSqlKey 7 :: LinkId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea "お疲れ様でした") (toSqlKey 7 :: LinkId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea "乙でしたー")     (toSqlKey 7 :: LinkId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Comment (Textarea "疲れた")         (toSqlKey 7 :: LinkId) (toSqlKey 1 :: PersonId)

    _ <- runDB $ deleteWhere ([] :: [Filter Event])

    redirect $ AdminLinksR


getAdminWorkspaceR :: Handler Html
getAdminWorkspaceR = do
    now <- liftIO getCurrentTime
    _ <- runDB $ deleteWhere ([] :: [Filter EventLog])
    _ <- runDB $ insert $ EventLog now 1 1 "Lorem ipsum dolor sit amet..." Nothing Nothing        (toSqlKey 1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ EventLog now 2 1 "深川二中"                      Nothing Nothing        (toSqlKey 1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ EventLog now 3 1 "深川二中"                      (Just 1) (Just "参加") (toSqlKey 1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ EventLog now 4 1 "録音"                          Nothing Nothing        (toSqlKey 1 :: GroupId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ EventLog now 5 7 "録音"                          (Just 9) (Just "乙")   (toSqlKey 3 :: GroupId) (toSqlKey 1 :: PersonId)

    events <- runDB $ selectList [] [Desc EventLogId]

    defaultLayout [whamlet|
        $forall Entity eid e <- events
            $case (eventLogPage e)
                $of 1
                    <p>
                        <a href=/messages/#{fromSqlKey $ eventLogGroupId e}#message-#{eventLogParentId e}>
                            #{eventLogParentContent e}
                        と発言しました
                $of 2
                    <p>
                        <a href=/schedules/#{fromSqlKey $ eventLogGroupId e}#schedule-#{eventLogParentId e}>
                            #{eventLogParentContent e}
                        を作成しました
                $of 3
                    <p>
                        <a href=/schedules/#{fromSqlKey $ eventLogGroupId e}#schedule-#{eventLogParentId e}>
                            #{eventLogParentContent e}
                        に
                        <a href=/schedule/detail/#{fromSqlKey $ eventLogGroupId e}/#{eventLogParentId e}#attendance-#{fromMaybe 0 $ eventLogChildId e}>
                            #{fromMaybe "" $ eventLogChildContent e}
                        と登録しました
                $of 4
                    <p>
                        <a href=/links/#{fromSqlKey $ eventLogGroupId e}#link-#{eventLogParentId e}>
                            #{eventLogParentContent e}
                        を作成しました
                $of 5
                    <p>
                        <a href=/links/#{fromSqlKey $ eventLogGroupId e}#link-#{eventLogParentId e}>
                            #{eventLogParentContent e}
                        に
                        <a href=/link/detail/#{fromSqlKey $ eventLogGroupId e}/#{eventLogParentId e}#comment-#{fromMaybe 0 $ eventLogChildId e}>
                            #{fromMaybe "" $ eventLogChildContent e}
                        とコメントしました
                $of _
    |]


postAdminDebugLoginR :: Handler Html
postAdminDebugLoginR = do
    mPersonIdText <- lookupPostParam "person-id"

    case mPersonIdText of
        (Just personIdText) -> do
            setSession "_ID" personIdText
            redirect $ HomeR
        Nothing -> error "login failed"
