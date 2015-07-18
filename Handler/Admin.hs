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

    now <- liftIO getNow
    let longBody = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    let shortBody = "Lorem ipsum dolor sit amet"
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

    _ <- runDB $ insert $ Link "アーノルドの楽譜" "http://home.n05.itscom.net/grotta/image/musicsheet/Thais.GIF" (toSqlKey 7 :: GroupId)
    _ <- runDB $ insert $ Link "録音（7/20）" "https://www.youtube.com/" (toSqlKey 7 :: GroupId)

    _ <- runDB $ insert $ Comment "アップしました" (toSqlKey 2 :: LinkId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Comment "あざますー"     (toSqlKey 2 :: LinkId) (toSqlKey 5 :: PersonId)
    _ <- runDB $ insert $ Comment "お疲れ様でした" (toSqlKey 2 :: LinkId) (toSqlKey 1 :: PersonId)
    _ <- runDB $ insert $ Comment "乙でしたー"     (toSqlKey 2 :: LinkId) (toSqlKey 4 :: PersonId)
    _ <- runDB $ insert $ Comment "疲れた"         (toSqlKey 2 :: LinkId) (toSqlKey 1 :: PersonId)

    redirect AdminR


getAdminWorkspaceR :: Handler Html
getAdminWorkspaceR = do
    defaultLayout [whamlet| workspace |]
