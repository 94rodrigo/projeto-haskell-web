{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cardapio where

import Import
import Handler.Auxiliar
import Text.Lucius
import Text.Julius


formCardapio :: Maybe Cardapio -> Form Cardapio
formCardapio mc = renderDivs $ Cardapio
    <$> areq textField "Nome do item: " (fmap cardapioNomeitem mc)
    <*> areq textField "Descrição: " (fmap cardapioDescricao mc)
    <*> areq doubleField "Preço: " (fmap cardapioPreco mc)

getItemCardapioR :: Handler Html
getItemCardapioR = do
    (widget, _) <- generateFormPost (formCardapio Nothing)
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageCadastro.lucius")
        $(whamletFile "templates/pageCadastro.hamlet")

postItemCardapioR :: Handler Html
postItemCardapioR = do
    ((result, _), _) <- runFormPost (formCardapio Nothing)
    case result of
        FormSuccess item -> do
            runDB $ insert item
            setMessage [shamlet|
                <div>
                    ITEM CADASTRADO COM SUCESSO
            |]
            redirect HomeR
        _ -> redirect ListaItensR

getItemR :: CardapioId -> Handler Html
getItemR iid = do
    cardapio <- runDB $ get404 iid
    defaultLayout [whamlet|
        <h1>
            Item do cardápio: #{cardapioNomeitem cardapio}
        
        <h2>
            Preço: #{cardapioPreco cardapio}
    |]

getListaItensR :: Handler Html
getListaItensR = do
    cardapio <- runDB $ selectList [] [Asc CardapioNomeitem]
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/cardapio.lucius")
        $(whamletFile "templates/cardapio.hamlet")

postApagarItemR :: CardapioId -> Handler Html
postApagarItemR iid = do
    runDB $ delete iid
    redirect ListaItensR

getEditarItemR :: CardapioId -> Handler Html
getEditarItemR iid = do
    item <- runDB $ get404 iid
    (widget, _) <- generateFormPost (formCardapio (Just item))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarItemR iid) "Editar")

postEditarItemR :: CardapioId -> Handler Html
postEditarItemR iid = do
    _ <- runDB $ get404 iid
    ((result,_),_) <- runFormPost (formCardapio Nothing)
    case result of
        FormSuccess novoItem -> do
            runDB $ replace iid novoItem
            redirect ListaItensR
        _ -> redirect HomeR

getListaComidaR :: Handler Html
getListaComidaR = do
    cardapio <- runDB $ selectList [] [Asc CardapioNomeitem]
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageEditarComida.lucius")
        $(whamletFile "templates/pageEditarComida.hamlet")