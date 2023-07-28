module Pages.ListFiles where

import Lucid

import Data.Foldable as F ()
import Pages.AddFile (addFileH)
import Pages.Result (resultH)
import Protolude hiding (for_)
import Types (Analysis (..), Config (..), FileName (..), NumberFormatKnown (..))

accordionH :: [(Html (), Html ())] -> Html ()
accordionH xs =
    div_ [class_ "accordion", id_ "mainAccordion"] $ do
        forM_ (zip [0 :: Int ..] xs) $ \(item, (k, v)) -> do
            div_ [class_ "accordion-item"] $ do
                div_ [class_ "accordion-header", id_ ("elem" <> show item)] $ do
                    h2_ [class_ "mb-0"] $ do
                        button_
                            [ class_ "accordion-button collapsed"
                            , type_ "button"
                            , data_ "bs-toggle" "collapse"
                            , data_ "bs-target" ("#collapse" <> show item)
                            , term "aria-expanded" "true"
                            , term "aria-controls" ("collapse" <> show item)
                            ]
                            k
                    div_
                        [ id_ ("collapse" <> show item)
                        , class_ "accordion-collapse collapse"
                        , term "aria-labelledby" ("elem" <> show item)
                        , term "data-parent" "#mainAccordion"
                        ]
                        $ do
                            div_ [class_ "accordion-body d-md-flex w-100"] v

listFilesH :: Text -> [(FileName, Analysis)] -> Html ()
listFilesH prefix files = do
    accordionH
        $ let
            items = do
                (FileName fn, analysis) <- files
                let k = do
                        case analysis of
                            FileAbsent -> do
                                span_ [class_ "badge bg-danger ms-2 me-2"] "Absent"
                            NotDone -> do
                                span_ [class_ "badge bg-warning ms-2 me-2 "] "Not done"
                            Configured _ -> do
                                span_ [class_ "badge bg-info ms-2 me-2"] "Configured"
                            Failed _ _ -> do
                                span_ [class_ "badge bg-danger ms-2 me-2"] "Failed"
                            Success _ _ -> do
                                span_ [class_ "badge bg-success ms-2 me-2"] "Success"
                        span_ [] $ h3_ $ toHtml fn
                    v = do
                        case analysis of
                            FileAbsent -> do
                                centeredColumn 8
                                    $ pure ()
                            NotDone -> do
                                centeredColumn 4
                                    $ configure prefix fn Nothing Nothing American
                                centeredColumn 4
                                    $ pure ()
                            Configured config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 4
                                    $ reAnalyze fn prefix
                            Failed _ config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 4
                                    $ configure prefix fn Nothing Nothing American
                            Success result config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 6
                                    $ resultH result
                pure (k, v)
            add = (toHtml @Text "add/replace file", addFileH prefix)
          in
            add : items

renderConfiguration :: Config -> Html ()
renderConfiguration (Config nf dateField amountField) = do
    dl_ [class_ "row"] $ do
        dt_ [class_ "col-md-3"] $ small_ "Number format"
        dd_ [class_ "col-md-9"] $ small_ $ toHtml @Text $ show nf
        dt_ [class_ "col-md-3"] $ small_ "Date field name"
        dd_ [class_ "col-md-9"] $ small_ $ toHtml dateField
        dt_ [class_ "col-md-3"] $ small_ "Amount field name"
        dd_ [class_ "col-md-9"] $ small_ $ toHtml amountField

centeredColumn :: Term [Attribute] result => Int -> result
centeredColumn n =
    div_
        [ class_
            $ "p-2 col-md-"
                <> show n
        ]

reAnalyze :: Text -> Text -> Html ()
reAnalyze fn prefix =
    div_ []
        $ form_
            [ method_ "POST"
            , action_ $ prefix <> "/file/analyze?filename=" <> fn
            , enctype_ "application/x-www-form-urlencoded"
            ]
        $ input_
            [ type_ "submit"
            , value_ "(Re)Analyze"
            , class_ "btn btn-primary"
            ]

numberFormatH :: NumberFormatKnown -> Html ()
numberFormatH nf = do
    div_ [class_ "form-check"] $ do
        input_
            $ [ type_ "radio"
              , id_ "number-format1"
              , name_ "number-format"
              , class_ "form-check-input"
              , value_ "american"
              ]
                <> ([term "checked" "checked" | nf == American])
        label_
            [for_ "number-format1", class_ "form-check-label"]
            "American number format"
    div_ [class_ "form-check"] $ do
        input_
            $ [ type_ "radio"
              , id_ "number-format2"
              , name_ "number-format"
              , class_ "form-check-input"
              , value_ "european"
              ]
                <> ([term "checked" "checked" | nf == European])
        label_
            [for_ "number-format2", class_ "form-check-label"]
            "European number format"

configure :: Text -> Text -> Maybe Text -> Maybe Text -> NumberFormatKnown -> Html ()
configure prefix filename mDateField mAmountField nf = do
    let dateField' = fromMaybe "Date" mDateField
    let amountField' = fromMaybe "Amount" mAmountField
    form_
        [ method_ "POST"
        , action_ $ prefix <> "/file/configure?filename=" <> filename
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
                    numberFormatH nf
            div_ [class_ "mb-3"] $ do
                input_
                    [ type_ "submit"
                    , value_ "Submit"
                    , class_ "btn btn-primary"
                    , required_ ""
                    ]
