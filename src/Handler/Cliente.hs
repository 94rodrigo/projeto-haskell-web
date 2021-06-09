{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import
import Handler.Auxiliar
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql

formCliente :: Form Cliente
formCliente = renderBootstrap $ Cliente
    <$> areq textField "Nome: (Apelido)" Nothing
    <*> areq textField "E-mail:" Nothing
    <*> areq intField "Celular: (DDD + Número do WhatsApp)" Nothing

getPaginaCadastroR :: Handler Html
getPaginaCadastroR = do
    (widget, _) <- generateFormPost formCliente
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageCadastro.lucius")
        $(whamletFile "templates/pageCadastro.hamlet")

postPaginaCadastroR :: Handler Html
postPaginaCadastroR = do
    ((result, _), _) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <script>
                    alert("Você entrou na fila. Aguarde, você será chamado via WhatsApp. Clique em Ok para continuar!");
            |]
            redirect ListaItensR
        _ -> redirect HomeR

-- FILA 
-- select * from cliente order by nome;
getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteId]
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/pageFila.hamlet")
        toWidgetHead $(luciusFile "templates/pagefila.lucius")
        
postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid 
    redirect ListaCliR
