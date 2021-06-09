{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger

    authRoute _ = Just AutR

    isAuthorized HomeR _ = return Authorized
    isAuthorized InstrucaoR _ = return Authorized
    isAuthorized (ComprarR _) _ = return Authorized
    isAuthorized (CarrinhoR _) _ = return Authorized
    isAuthorized SobreR _ = return Authorized 
    isAuthorized GerenciaComidaR _ = isAdmin
    isAuthorized ListaComidaR _ = isAdmin
    isAuthorized ListaCliR _ = isUsuario
    isAuthorized ListaItensR _ = return Authorized
    isAuthorized (ApagarCliR _) _ = isUsuario
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized AutR _ = return Authorized
    isAuthorized UsuarioR _ = isAdmin
    isAuthorized PaginaCadastroR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized FuncoesAdminR _ = isAdmin
    isAuthorized ListaUseR _ = isAdmin
    isAuthorized _ _ = isUsuario

isAdmin :: Handler AuthResult
isAdmin = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just "admin" -> return Authorized
        Just _ -> return (Unauthorized "Acesso restrito para o administrador")

isUsuario :: Handler AuthResult
isUsuario = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
