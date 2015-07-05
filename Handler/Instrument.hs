module Handler.Instrument where


import Import


form :: Maybe Instrument -> Html -> MForm Handler (FormResult Instrument, Widget)
form mEntity extra = do
    (nameResult, nameView) <- mreq textField "" (instrumentName <$> mEntity)
    (iconResult, iconView) <- mreq textField "" (instrumentIcon <$> mEntity)
    (codeResult, codeView) <- mreq textField "" (instrumentCode <$> mEntity)
    let result = Instrument
           <$> nameResult
           <*> iconResult
           <*> codeResult
        widget = $(widgetFile "instrument/form")
    return (result, widget)


getInstrumentListR :: Handler Html
getInstrumentListR = do
    entities <- runDB $ selectList [] [Asc InstrumentId]

    defaultLayout $(widgetFile "instrument/list")


getInstrumentCreateR :: Handler Html
getInstrumentCreateR = do
    (widget, enctype) <- generateFormPost $ form Nothing

    defaultLayout $(widgetFile "instrument/create")


postInstrumentCreateR :: Handler Html
postInstrumentCreateR = do
    ((res, widget), enctype) <- runFormPost $ form Nothing
    case res of
        FormSuccess entity -> do
            groupId <- runDB $ insert entity
            redirect $ InstrumentListR

        _ -> defaultLayout $ do
            $(widgetFile "instrument/create")


getInstrumentUpdateR :: InstrumentId -> Handler Html
getInstrumentUpdateR id = do
    entity <- runDB $ get404 id
    (widget, enctype) <- generateFormPost $ form (Just entity)

    defaultLayout $(widgetFile "instrument/update")


postInstrumentUpdateR :: InstrumentId -> Handler Html
postInstrumentUpdateR id = do
    ((res, widget), enctype) <- runFormPost $ form Nothing
    case res of
        FormSuccess entity -> do
            runDB $ replace id entity
            redirect $ InstrumentListR

        _ -> defaultLayout $ do
            $(widgetFile "instrument/update")


postInstrumentDeleteR :: InstrumentId -> Handler Html
postInstrumentDeleteR id = do
    runDB $ delete id
    redirect $ InstrumentListR
