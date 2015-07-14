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
    _ <- runDB $ insert $ Person "user.ryo@gmail.com" "鈴木" "hoge.png"
    _ <- runDB $ insert $ Person "m-i.com" "松本" "default_1.png"
    _ <- runDB $ insert $ Person "t-h.com" "千葉" "default_2.png"
    _ <- runDB $ insert $ Person "i-k.com" "伊藤" "default_1.png"
    _ <- runDB $ insert $ Person "m-y.com" "宮本" "default_2.png"
    _ <- runDB $ insert $ Person "o-j.com" "大瀧" "default_1.png"

    _ <- runDB $ insert $ Group "東京ブラスオルケスター"           "tcbo.jpg"
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

    _ <- runDB $ insert $ Message "東ブラほげ〜"                       (fromGregorian 2015 7 14) (toSqlKey 1 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう"         (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                         (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう"         (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                         (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう"         (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                         (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう"         (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                         (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "千葉さんタバコ行きましょう"         (fromGregorian 2015 7 14) (toSqlKey 2 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "いいですね"                         (fromGregorian 2015 7 14) (toSqlKey 3 :: PersonId) (toSqlKey 1 :: GroupId)
    _ <- runDB $ insert $ Message "年末の同窓会が楽しみだ〜"           (fromGregorian 2015 7 14) (toSqlKey 1 :: PersonId) (toSqlKey 3 :: GroupId)
    _ <- runDB $ insert $ Message "アーノルド難しかったけど楽しかった" (fromGregorian 2015 7 14) (toSqlKey 1 :: PersonId) (toSqlKey 7 :: GroupId)
    _ <- runDB $ insert $ Message "お疲れっしたー"                     (fromGregorian 2015 7 14) (toSqlKey 4 :: PersonId) (toSqlKey 7 :: GroupId)

    redirect AdminR
