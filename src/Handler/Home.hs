{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Auxiliar
import Text.Lucius
import Text.Julius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql


-- Monad Handler => backend
-- Monad Widget => frontend
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")

getInstrucaoR :: Handler Html
getInstrucaoR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageInstrucao.lucius")
        $(whamletFile "templates/pageInstrucao.hamlet")

getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageSobre.lucius")
        $(whamletFile "templates/pageSobre.hamlet")