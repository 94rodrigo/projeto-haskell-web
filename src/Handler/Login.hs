{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Handler.Auxiliar
import Handler.Cardapio
import Text.Lucius
import Text.Julius

formLogin :: Form Usuario
formLogin = renderBootstrap $ Usuario
    <$> areq textField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getAutR :: Handler Html
getAutR = do
    (widget, _) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageLogin.lucius")
        $(whamletFile "templates/pageLogin.hamlet")

postAutR :: Handler Html
postAutR = do
    ((result, _), _) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario "admin@admin.com" "admin") -> do
            setSession "_ID" "admin"
            redirect HomeR
        FormSuccess (Usuario email senha) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do
                    setMessage [shamlet|
                        <script>
                            alert("Usuario não cadastrado");
                    |]
                    redirect AutR
                Just (Entity _ usuario) -> do
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (usuarioEmail usuario)
                        redirect HomeR
                    else
                        setMessage [shamlet|
                            <script>
                                alert("Usuario e/ou senha preenchidos incorretamente");
                            
                    |]
                    redirect AutR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
        BEM-VINDO, ADMINISTRADOR!
    |]


getFuncoesAdminR :: Handler Html
getFuncoesAdminR = do
    (widget, _) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/FuncAdmin.lucius")
        $(whamletFile "templates/FuncAdmin.hamlet")


postFuncoesAdminR :: Handler Html
postFuncoesAdminR = do
    ((result, _), _) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario "admin@admin.com" "admin") -> do
            setSession "_ID" "admin"
            redirect FuncoesAdminR


getGerenciaComidaR :: Handler Html
getGerenciaComidaR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageGerenciaComida.lucius")
        $(whamletFile "templates/pageGerenciaComida.hamlet")

