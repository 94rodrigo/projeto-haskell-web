{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Venda where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql

formVenda :: ClienteId -> Form Pedido 
formVenda cid = renderBootstrap $ Pedido
    <$> pure cid
    <*> areq (selectField prodCB) "Produto: "Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq intField  "Quantidade: " Nothing

-- [(Lapis,Key 1),(Borracha,Key 2),...]
prodCB :: Handler (OptionList (Key Cardapio))
prodCB = do
  cardapios <- runDB $ selectList [] [Asc CardapioNomeitem]
  optionsPairs $ 
      map (\r -> (cardapioNomeitem $ entityVal r, entityKey r)) cardapios    
    
getComprarR :: ClienteId -> Handler Html
getComprarR cid = do
    (widget,_) <- generateFormPost (formVenda cid)
    msg <- getMessage
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageCompra.lucius")
        $(whamletFile "templates/pageCompra.hamlet")

postComprarR :: ClienteId -> Handler Html
postComprarR cid = do
    ((result,_),_) <- runFormPost (formVenda cid)
    case result of 
        FormSuccess venda -> do 
            runDB $ insert venda 
            setMessage [shamlet|
                <script>
                   Alert("Pedido incluido com sucesso!! clique em OK!");
            |]
            redirect (CarrinhoR cid)
        _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)
     
getCarrinhoR :: ClienteId -> Handler Html
getCarrinhoR cid = do 
    let sql = "SELECT ??,??,?? FROM cardapio \
          \ INNER JOIN pedido ON pedido.prodid = cardapio.id \
          \ INNER JOIN cliente ON pedido.cliid = cliente.id \
          \ WHERE cliente.id = ?"
    cliente <- runDB $ get404 cid
    tudo <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Cardapio,Entity Pedido,Entity Cliente)]
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/pageCarrinho.lucius")
        $(whamletFile "templates/pageCarrinho.hamlet")
        
        {-[whamlet|
            <h1>
                PEDIDO DE #{clienteNome cliente}
            <ul>
                $forall (Entity _ cardapio, Entity _ pedido, Entity _ _) <- tudo
                    <li>
                        #{cardapioNomeitem cardapio}, #{mult (cardapioPreco cardapio) (fromIntegral (pedidoQt pedido))} no dia #{show $ pedidoDia pedido}
        |]-}
