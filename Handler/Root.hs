module Handler.Root where

import Import


getRootR :: Handler Html
getRootR = do
    defaultLayout [whamlet| top page |]
