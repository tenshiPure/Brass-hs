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
    defaultLayout [whamlet| #{show personId} |]
--     instrumentIds <- getInstrumentIds
--     years <- getYears
--     let experiences = zipWith (\instrumentId year -> Experience personId instrumentId year) instrumentIds years
--     map (runDB . insert) experiences
--     redirect $ ExperienceUpdateR personId
-- 
-- getInstrumentIds :: HandlerT App IO [InstrumentId]
-- getInstrumentIds = do
--     instrumentIds' <- lookupPostParams "instrument-id[]"
--     return $ map (read . unpack) instrumentIds'
-- 
-- getYears :: (MonadHandler m, Read a) => m [a]
-- getYears = do
--     years' <- lookupPostParams "years[]"
--     return $ map (read . unpack) years'

-- getExperience :: PersonId -> Experience
-- getExperience personId = Experience personId (read "InstrumentKey {unInstrumentKey = SqlBackendKey {unSqlBackendKey = 2}}" :: InstrumentId) 0
