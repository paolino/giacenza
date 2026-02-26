module Pages.Page where

import Control.Lens.Extras (is)
import Lucid
    ( Attribute
    , Html
    , Term (term)
    , ToHtml (toHtml)
    , a_
    , body_
    , charset_
    , class_
    , content_
    , crossorigin_
    , div_
    , h5_
    , head_
    , header_
    , href_
    , html_
    , integrity_
    , li_
    , link_
    , meta_
    , name_
    , p_
    , rel_
    , renderBS
    , span_
    , src_
    , title_
    , ul_
    )
import Pages.AddFile (addFileH)
import Pages.ListFiles (listFilesH)
import Pages.Types
    ( Page (..)
    , RawHtml (RawHtml)
    , _ListFiles
    )
import Protolude hiding (for_)
import Types (Config, FileName, Result)

pageH :: Text -> Page -> Html () -> Html ()
pageH prefix p body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ "Average deposit and end-of-year balance of your transactions"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_
                [ rel_ "stylesheet"
                , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
                , integrity_
                    "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM"
                , crossorigin_ "anonymous"
                ]
            term
                "script"
                [ src_
                    "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
                , integrity_
                    "sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz"
                , crossorigin_ "anonymous"
                ]
                $ pure ()

            pure ()
        body_ $ do
            headH p prefix
            div_ [class_ "container-fluid"] $ do
                div_
                    [class_ "main"]
                    body
                div_
                    [class_ "footer"]
                    footerH

activePageH :: Bool -> [Attribute] -> [Attribute]
activePageH c =
    if c
        then (<> [class_ "nav-link active", term "aria-current" "page"])
        else (<> [class_ "nav-link"])

headH :: Page -> Text -> Html ()
headH p prefix = do
    header_ [class_ "d-flex justify-content-center py-3"] $ do
        ul_ [class_ "nav nav-pills"] $ do
            li_ [class_ "nav-item"] $ do
                a_
                    ( activePageH
                        (is _ListFiles p)
                        [href_ $ prefix <> "/file/all"]
                    )
                    "Statements"
            li_ [class_ "nav-item"] $ do
                a_
                    [class_ "nav-link", href_ "https://paolino.github.io/giacenza/"]
                    "Docs"

page
    :: Maybe FileName
    -> Map FileName Config
    -> Result
    -> Text
    -> Page
    -> RawHtml
page focus mcfg sums prefix p = RawHtml $ renderBS $ case p of
    ListFiles as -> pageH prefix p $ do
        listFilesH focus mcfg prefix as sums
    AddFile -> pageH prefix p $ addFileH prefix

footerH :: Html ()
footerH =
    term
        "footer_"
        [ class_
            "d-md-flex flex-wrap justify-content-between align-items-center py-3 ms-4 border-top"
        ]
        $ do
            div_ [class_ "col d-md-flex align-items-center"]
                $ do
                    ul_ [class_ "nav flex-column"] $ do
                        li_ [class_ "nav-item mb-2"] "© 2023-2026 Paolo Veronelli, Lambdasistemi"
                        li_ [class_ "nav-item mb-2"] $ do
                            span_ "Source code on "
                            a_
                                [href_ "https://github.com/paolino/giacenza"]
                                "GitHub"
            div_ [class_ "col d-md-flex align-items-center"]
                $ div_
                    [class_ "mb-3 mb-md-0 text-body-secondary"]
                    "Powered by Haskell, Servant, Polysemy, Lucid, Streaming, Bootstrap"

reportExcH :: Text -> Html ()
reportExcH msg = do
    -- class="alert alert-primary" role="alert"
    div_ [class_ "alert alert-danger", term "role_" "alert"] $ do
        h5_ "Error in the request"
        p_ $ toHtml msg
