module Handler.Home where


import Import


getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet| hello |]
