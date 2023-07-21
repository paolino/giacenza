{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Form where

import Protolude hiding (for_)

import Compute (showEuro)
import Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.Map.Strict as Map
import Lucid
    ( Html
    , Term (term)
    , ToHtml (toHtml)
    , a_
    , action_
    , body_
    , charset_
    , class_
    , content_
    , crossorigin_
    , div_
    , enctype_
    , for_
    , form_
    , h3_
    , h4_
    , h5_
    , head_
    , header_
    , href_
    , html_
    , id_
    , input_
    , integrity_
    , label_
    , li_
    , link_
    , meta_
    , method_
    , name_
    , novalidate_
    , p_
    , rel_
    , renderBS
    , required_
    , src_
    , table_
    , td_
    , th_
    , title_
    , tr_
    , type_
    , ul_
    , value_, span_
    )
import Network.HTTP.Media ((//), (/:))
import Servant (MimeRender (..))
import Servant.API (Accept (..))
import Types
    ( Giacenza (Giacenza)
    , NumberFormatKnown (..)
    , Result (..)
    , Saldo (Saldo)
    , Year (Year)
    )

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

pageH :: Html () -> Html ()
pageH body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ "Analisi deposit media"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_
                [ rel_ "stylesheet"
                , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
                , integrity_ "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM"
                , crossorigin_ "anonymous"
                ]
            term
                "script"
                [ src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
                , integrity_ "sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz"
                , crossorigin_ "anonymous"
                ]
                $ pure ()

            pure ()
        body_ $ do
            headH
            div_ [class_ "container"] $ do
                div_
                    [class_ "main"]
                    body
                div_
                    [class_ "footer"]
                    footerH

renderNumberFormat :: NumberFormatKnown -> Text
renderNumberFormat European = "european"
renderNumberFormat American = "american"

radioH :: NumberFormatKnown -> Html ()
radioH nf = do
    div_ [class_ "form-check"] $ do
        input_
            $ [ type_ "radio"
              , id_ "number-format1"
              , name_ "number-format"
              , class_ "form-check-input"
              , value_ "american"
              ]
                <> ([term "checked" "checked" | nf == American])
        label_ [for_ "number-format1", class_ "form-check-label"] "American number format"
    div_ [class_ "form-check"] $ do
        input_
            $ [ type_ "radio"
              , id_ "number-format2"
              , name_ "number-format"
              , class_ "form-check-input"
              , value_ "european"
              ]
                <> ([term "checked" "checked" | nf == European])
        label_ [for_ "number-format2", class_ "form-check-label"] "European number format"

formH :: Maybe Text -> Maybe Text -> NumberFormatKnown -> Html ()
formH mDateField mAmountField nf = div_ [] $ do
    let dateField' = fromMaybe "Date" mDateField
    let amountField' = fromMaybe "Amount" mAmountField
    h3_ [class_ "mb-3 border-bottom"] "New request"
    form_
        [ method_ "POST"
        , action_ "/deposit/form"
        , enctype_ "multipart/form-data"
        , class_ "needs-validation"
        , novalidate_ ""
        ]
        $ do
            div_ [class_ "mb-3 row"] $ do
                div_ [class_ "mb-3 col"] $ do
                    label_
                        [for_ "date-name", class_ "form-label"]
                        "Date field name:"
                    input_
                        [ type_ "text"
                        , id_ "date-name"
                        , name_ "date-name"
                        , value_ dateField'
                        , class_ "form-control"
                        , required_ ""
                        ]
                div_ [class_ "mb-3 col"] $ do
                    label_
                        [for_ "amount-name", class_ "form-label"]
                        "Amount field name:"
                    input_
                        [ type_ "text"
                        , id_ "amount-name"
                        , name_ "amount-name"
                        , value_ amountField'
                        , class_ "form-control"
                        , required_ ""
                        ]
            div_ [class_ "mb-3 row"] $ do
                div_ [class_ "mb-3 col"] $ do
                    radioH nf
            div_ [class_ "mb-3"] $ do
                label_
                    [for_ "upload", class_ "form-label"]
                    "CSV file:"
                input_ [type_ "file", id_ "csv-data", name_ "filename", class_ "form-control"]
            div_ [class_ "mb-3"] $ do
                input_
                    [ type_ "submit"
                    , value_ "Submit"
                    , class_ "btn btn-primary"
                    , required_ ""
                    ]

resultH :: Text -> Result -> Html ()
resultH filename (Result m) =
    div_
        $ do
            h3_ [class_ "mb-3 border-bottom"] "Analysys"
            h5_ $ toHtml @Text filename
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

headH :: Html ()
headH = do
    header_ [class_ "d-flex justify-content-center py-3"] $ do
        ul_ [class_ "nav nav-pills"] $ do
            li_ [class_ "nav-item"] $ do
                a_ [href_ "/", class_ "nav-link active", term "aria-current" "page"] "Home"
            li_ [class_ "nav-item"] $ do
                a_ [href_ "/about", class_ "nav-link"] "About"

aboutH :: Html ()
aboutH = do
    h4_
        "About"
    p_
        "This is a simple web application to compute the average deposit and the end of the year balance \
        \ of a bank account from a csv file.The average deposit is the daily average of the deposit. \
        \ The balance is the deposited value at the last day of the year."
    ul_ $ do
        li_
            "The csv file must have a header with the date and the amount fields, with names chosen \
            \ in the form and must contain all the movements of the account history."
        li_ "The date field must be in the format dd-mm-yyyy."
        li_ "The amount field must be a number, with the decimal separator chosen in the form."

    p_ "The result is a table with the average deposit and the balance for each year."

data Feedback = Feedback
    { dateField :: Text
    , amountField :: Text
    , numberFormat :: NumberFormatKnown
    , clientFilename :: Text
    }
data Page = Home | About | Positive Feedback Result | Negative Feedback Text

page :: Page -> RawHtml
page p = RawHtml $ renderBS $ case p of
    Home -> pageH $ formH Nothing Nothing European
    About -> pageH aboutH
    Positive Feedback{..} result -> pageH $ do
        div_ [class_ "row"]
            $ resultH clientFilename result
        div_ [class_ "row mt-5"]
            $ formH (Just dateField) (Just amountField) numberFormat
    Negative Feedback{..} msg -> pageH $ do
        div_ [class_ "row"]
            $ reportExcH msg
        div_ [class_ "row mt-5"]
            $ formH (Just dateField) (Just amountField) numberFormat

footerH :: Html ()
footerH =
    term "footer_" [class_ "d-flex flex-wrap justify-content-between align-items-center py-3 my-4 border-top"]
        $ do
            div_ [class_ "col d-flex align-items-center"]
                $ do
                    ul_ [class_ "nav flex-column"] $ do
                        li_ [class_ "nav-item mb-2"] "© 2023 Paolo Veronelli, Lambdasistemi"
                        li_ [class_ "nav-item mb-2"] $ do 
                            span_ "Source code on "
                            a_
                                [href_ "https://github.com/paolino/giacenza"]
                                "GitHub"
            div_ [class_ "col d-flex align-items-center"]
                $ div_ [class_ "mb-3 mb-md-0 text-body-secondary"] "Powered by Haskell, Servant, Lucid, Streaming, Bootstrap"

reportExcH :: Text -> Html ()
reportExcH msg = do
    -- class="alert alert-primary" role="alert"
    div_ [class_ "alert alert-danger", term "role_" "alert"] $ do
        h4_ "Error in the request"
        p_ $ toHtml msg
