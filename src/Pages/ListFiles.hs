module Pages.ListFiles where

import Lucid

import Control.Lens ((^..), _1)
import Data.Foldable as F ()
import Data.List (elemIndex)
import Pages.AddFile (addFileH, deleteFileH, reconfigureFileH)
import Pages.Result (resultH)
import Protolude hiding (for_)
import Types (Analysis (..), Config (..), FileName (..), NumberFormatKnown (..))

accordionH :: Maybe Int -> [(Html (), Html ())] -> Html ()
accordionH focus xs =
    div_ [class_ "accordion", id_ "mainAccordion"] $ do
        forM_ (zip [0 :: Int ..] xs) $ \(item, (k, v)) -> do
            div_ [class_ "accordion-item"] $ do
                div_ [class_ "accordion-header", id_ ("elem" <> show item)] $ do
                    h4_ [class_ "mb-0"] $ do
                        button_
                            [ class_
                                $ "accordion-button"
                                    <> if Just item == focus then "" else " collapsed"
                            , type_ "button"
                            , data_ "bs-toggle" "collapse"
                            , data_ "bs-target" ("#collapse" <> show item)
                            , term "aria-expanded" "true"
                            , term "aria-controls" ("collapse" <> show item)
                            ]
                            k
                    div_
                        [ id_ ("collapse" <> show item)
                        , class_
                            $ "accordion-collapse collapse"
                                <> if Just item == focus then " show" else ""
                        , term "aria-labelledby" ("elem" <> show item)
                        , term "data-parent" "#mainAccordion"
                        ]
                        $ do
                            div_ [class_ "accordion-body d-md-flex w-100"] v

listFilesH :: Maybe FileName -> Text -> [(FileName, [Text], Analysis)] -> Html ()
listFilesH focus prefix files = do
    accordionH (focus >>= \fn -> elemIndex fn (files ^.. traverse . _1) <&> succ)
        $ let
            items = do
                (FileName fn, header, analysis) <- files
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
                            Unconfigurable _ -> do
                                span_ [class_ "badge bg-danger ms-2 me-2"] "Unconfigurable"

                        span_ [] $ h4_ $ toHtml fn
                    v = do
                        case analysis of
                            FileAbsent -> do
                                centeredColumn 8
                                    $ pure ()
                            NotDone -> do
                                centeredColumn 4
                                    $ configure prefix fn header American
                                centeredColumn 4
                                    $ buttons
                                    $ deleteFileH prefix fn
                            Configured config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 4
                                    $ reAnalyze fn prefix
                            Failed _ config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 4
                                    $ buttons
                                    $ do
                                        reconfigureFileH prefix fn
                                        deleteFileH prefix fn
                            Success result config -> do
                                centeredColumn 4
                                    $ renderConfiguration config
                                centeredColumn 4
                                    $ resultH result
                                centeredColumn 4
                                    $ buttons
                                    $ do
                                        reconfigureFileH prefix fn
                                        deleteFileH prefix fn
                            Unconfigurable f -> do
                                centeredColumn 4
                                    $ toHtml @Text
                                    $ show f
                                centeredColumn 4
                                    $ configure prefix fn header American
                                centeredColumn 4
                                    $ buttons
                                    $ deleteFileH prefix fn

                pure (k, v)
            add =
                ( h4_ [class_ "text-center"] $ toHtml @Text "add/replace file"
                , addFileH prefix
                )
          in
            add : items

buttons :: Html () -> Html ()
buttons =
    div_
        [ class_
            "justify-content-end d-flex align-items-end"
        ]

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

configure :: Text -> Text -> [Text] -> NumberFormatKnown -> Html ()
configure prefix filename header nf = do
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
                    selectHeader "date-name" header
                div_ [class_ "mb-3 col"] $ do
                    label_
                        [for_ "amount-name", class_ "form-label"]
                        "Amount field name:"
                    selectHeader "amount-name" header

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

selectHeader :: Text -> [Text] -> Html ()
selectHeader name header = do
    label_
        [for_ name, class_ "form-label"]
        "Header:"
    select_
        [ class_ "form-select"
        , id_ name
        , name_ name
        , required_ ""
        , term "size" $ show $ length header
        ]
        $ do
            forM_ header $ \h -> do
                option_
                    [ value_ h
                    , term "selected" "selected"
                    ]
                    $ toHtml h
