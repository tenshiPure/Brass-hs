module Handler.Experience where


import Import


data Es = Es { ePersonId :: PersonId, eInstrumentId :: InstrumentId, eMaybeYears :: Maybe Int, eInstrumentName ::  Text }

getExperienceListR :: PersonId -> Handler Html
getExperienceListR personId = do
    person <- runDB $ get404 personId
    instruments <- runDB $ selectList [] [Asc InstrumentId]

    es <- forM instruments $ \instrument -> do
        let instrumentId' = entityKey instrument
        let instrumentName' = instrumentName $ entityVal instrument
        e <- runDB $ selectFirst [ExperiencePersonId ==. personId, ExperienceInstrumentId ==. instrumentId'] [Asc ExperienceId]
        let years' = experienceYears . entityVal <$> e

        return $ Es personId instrumentId' years' instrumentName'

    defaultLayout $(widgetFile "experience/list")
