module Handler.Experience where


import Import


data Es = Es { ePersonId :: PersonId, eInstrumentId :: InstrumentId, eMaybeYears :: Maybe Int, eInstrumentName ::  Text, eInstrumentIcon :: Text }


getExperienceUpdateR :: PersonId -> Handler Html
getExperienceUpdateR personId = do
    person <- runDB $ get404 personId
    instruments <- runDB $ selectList [] [Asc InstrumentId]

    es <- forM instruments $ \instrument -> do
        let instrumentId' = entityKey instrument
        let instrumentName' = instrumentName $ entityVal instrument
        let instrumentIcon' = instrumentIcon $ entityVal instrument
        e <- runDB $ selectFirst [ExperiencePersonId ==. personId, ExperienceInstrumentId ==. instrumentId'] [Asc ExperienceId]
        let years' = experienceYears . entityVal <$> e

        return $ Es personId instrumentId' years' instrumentName' instrumentIcon'

    defaultLayout $(widgetFile "experience/list")


postExperienceUpdateR :: PersonId -> Handler Html
postExperienceUpdateR personId = do
    defaultLayout [whamlet| hello |]
