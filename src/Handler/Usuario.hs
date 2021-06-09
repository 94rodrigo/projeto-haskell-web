{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Handler.Auxiliar
import Text.Lucius
import Text.Julius

formLogin :: Form (Usuario, Text)
formLogin = renderBootstrap $ (,)
    <$> (Usuario
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing
        )
    <*> areq passwordField "Confirme sua senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget, _) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageUsuario.lucius")
        $(whamletFile "templates/pageUsuario.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result, _), _) <- runFormPost formLogin
    case result of
        FormSuccess (usuario@(Usuario email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <script>
                            alert("E-mail já cadastrado!");
                    |]
                    redirect UsuarioR
                Nothing -> do
                    if senha == conf then do
                        runDB $ insert usuario
                        setMessage [shamlet|
                            <script>
                                alert("Usuário cadastrado com Sucesso!");
                        |]
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <script>
                                alert("Senha e confirmação de senha estão diferentes!");
                        |]
                        redirect UsuarioR
        _ -> redirect HomeR

-- select * from cliente order by nome;
getListaUseR :: Handler Html
getListaUseR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioId]
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/pageGerenciaUsuario.hamlet")
        toWidgetHead $(luciusFile "templates/pageGerenciaUsuario.lucius")
        
postApagarUseR :: UsuarioId -> Handler Html
postApagarUseR cid = do
    runDB $ delete cid 
    redirect ListaUseR