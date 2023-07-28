module Pages.Result where

import Compute (showEuro)
import Data.Map.Strict qualified as Map
import Lucid
    ( Html
    , ToHtml (toHtml)
    , class_
    , table_
    , td_
    , th_
    , tr_
    )
import Protolude hiding (for_)
import Types
    ( Giacenza (Giacenza)
    , Result (..)
    , Saldo (Saldo)
    , Year (Year)
    )

resultH :: Result -> Html ()
resultH (Result m) = do
    table_ [class_ "table table-striped"] $ do
        tr_ $ do
            th_
                [class_ "text-end"]
                "Year"
            th_
                [class_ "text-end"]
                "Average"
            th_
                [class_ "text-end"]
                "Last balance"
        forM_ (Map.assocs m) $ \(Year y, (Saldo s, Giacenza g)) -> do
            tr_ $ do
                td_ [class_ "text-end"]
                    $ toHtml @Text
                    $ show y
                td_ [class_ "text-end"]
                    $ toHtml
                    $ showEuro g
                td_ [class_ "text-end"]
                    $ toHtml
                    $ showEuro s
