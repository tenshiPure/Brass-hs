module Handler.Admin where


import Import
import Database.Persist.Sql(toSqlKey)


getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
    <ul>
        <li>
            <a href=@{PersonListR}>
                人一覧
        <li>
            <a href=@{GroupListR}>
                グループ一覧
        <hr>

        <li>
            <a href=/auth/login>
                ログイン
        <li>
            <a href=@{AdminInitDBR}>
                DB初期化
    |]


getAdminInitDBR :: Handler Html
getAdminInitDBR = do
    _ <- runDB $ insert $ Person "user.ryo@gmail.com" "鈴木"
    _ <- runDB $ insert $ Person "m-i.com" "松本"
    _ <- runDB $ insert $ Person "t-h.com" "千葉"
    _ <- runDB $ insert $ Person "i-k.com" "伊藤"
    _ <- runDB $ insert $ Person "m-y.com" "宮本"
    _ <- runDB $ insert $ Person "o-j.com" "大瀧"

    _ <- runDB $ insert $ Group "東京ブラスオルケスター"
    _ <- runDB $ insert $ Group "江戸川管楽合奏団"
    _ <- runDB $ insert $ Group "高輪台高等学校吹奏楽部"
    _ <- runDB $ insert $ Group "新交響楽団"
    _ <- runDB $ insert $ Group "東京隆生吹奏楽団"
    _ <- runDB $ insert $ Group "マシュアールウィンドオーケストラ"
    _ <- runDB $ insert $ Group "伊藤マリーンズ"
    _ <- runDB $ insert $ Group "東京スタックアート吹奏楽団"
    _ <- runDB $ insert $ Group "創価グロリア吹奏楽団"
    _ <- runDB $ insert $ Group "ミュゼダール吹奏楽団"
    _ <- runDB $ insert $ Group "土気シビックウィンドオーケストラ"
    _ <- runDB $ insert $ Group "ダミーグループ - 01"
    _ <- runDB $ insert $ Group "ダミーグループ - 02"
    _ <- runDB $ insert $ Group "ダミーグループ - 03"
    _ <- runDB $ insert $ Group "ダミーグループ - 04"
    _ <- runDB $ insert $ Group "ダミーグループ - 05"
    _ <- runDB $ insert $ Group "ダミーグループ - 06"
    _ <- runDB $ insert $ Group "ダミーグループ - 07"
    _ <- runDB $ insert $ Group "ダミーグループ - 08"
    _ <- runDB $ insert $ Group "ダミーグループ - 09"
    _ <- runDB $ insert $ Group "ダミーグループ - 10"
    _ <- runDB $ insert $ Group "ダミーグループ - 11"
    _ <- runDB $ insert $ Group "ダミーグループ - 12"
    _ <- runDB $ insert $ Group "ダミーグループ - 13"
    _ <- runDB $ insert $ Group "ダミーグループ - 14"
    _ <- runDB $ insert $ Group "ダミーグループ - 15"
    _ <- runDB $ insert $ Group "ダミーグループ - 16"
    _ <- runDB $ insert $ Group "ダミーグループ - 17"

    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey  3 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey  7 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 12 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 13 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 14 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 15 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 16 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 17 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 18 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 19 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 20 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 21 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 22 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 23 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 24 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 25 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 26 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 27 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 1 :: PersonId) (toSqlKey 28 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 2 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 2 :: PersonId) (toSqlKey  4 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 3 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 3 :: PersonId) (toSqlKey  2 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 4 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 4 :: PersonId) (toSqlKey  2 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 4 :: PersonId) (toSqlKey  7 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 5 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 5 :: PersonId) (toSqlKey  2 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 5 :: PersonId) (toSqlKey  7 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 6 :: PersonId) (toSqlKey  1 :: GroupId)
    _ <- runDB $ insert $ Belong (toSqlKey 6 :: PersonId) (toSqlKey  3 :: GroupId)

    _ <- runDB $ insert $ Message "東ブラほげ〜"               (fromGregorian 2015 7 14) (toSqlKey 1 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう" (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                 (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう" (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                 (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう" (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                 (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう" (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                 (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう" (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                 (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)

    redirect AdminR
