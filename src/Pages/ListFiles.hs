module Pages.ListFiles where

import Lucid

import Control.Lens ((^..), _1)
import Data.Foldable as F ()
import Data.List (elemIndex)
import Data.Map qualified as Map (lookup)
import Pages.AddFile (addFileH, deleteFileH, reconfigureFileH)
import Pages.Result (resultH)
import Protolude hiding (for_)
import Types (Analysis (..), Config (..), FileName (..), NumberFormatKnown (..), Result)

accordionH :: Maybe Int -> [(Html (), Html ())] -> Html ()
accordionH focus xs =
    div_ [class_ "accordion", id_ "mainAccordion"] $ do
        forM_ (zip [0 :: Int ..] xs) $ \(item, (k, v)) -> do
            div_ [class_ "accordion-item"] $ do
                div_ [class_ "accordion-header", id_ ("elem" <> show item)] $ do
                    h5_ [class_ "mb-0"] $ do
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

listFilesH
    :: Maybe FileName
    -> Map FileName Config
    -> Text
    -> [(FileName, [Text], Analysis)]
    -> Result
    -> Html ()
listFilesH focus cfg prefix files sums = do
    accordionH (focus >>= \fn -> elemIndex fn (files ^.. traverse . _1))
        $ let
            items = do
                (filename@(FileName fn), header, analysis) <- files
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

                        span_ [] $ h5_ $ toHtml fn
                    v = do
                        case analysis of
                            FileAbsent -> do
                                centeredColumn 8
                                    $ pure ()
                            NotDone -> do
                                centeredColumn 4
                                    $ configure prefix fn header
                                    $ Map.lookup filename cfg
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
                                    $ buttons
                                    $ deleteFileH prefix fn

                pure (k, v)
            final =
                ( h5_ [class_ "text-center"] $ toHtml @Text "Final report"
                , resultH sums
                )

            add =
                ( h5_ [class_ "text-center"] $ toHtml @Text "Add files"
                , addFileH prefix
                )
            global =
                ( h5_ [class_ "text-center"] $ toHtml @Text "Global changes"
                , buttons $ reconfigureAll prefix >> deleteAll prefix
                )
          in
            items <> [final, add, global]

buttons :: Html () -> Html ()
buttons =
    div_
        [ class_
            "d-grid gap-2 d-md-flex justify-content-md-end"
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

deleteAll :: Text -> Html ()
deleteAll prefix =
    div_ []
        $ form_
            [ method_ "POST"
            , action_ $ prefix <> "/file/delete-all"
            ]
        $ input_
            [ type_ "submit"
            , value_ "Delete all files"
            , class_ "btn btn-danger"
            ]

reconfigureAll :: Text -> Html ()
reconfigureAll prefix =
    div_ []
        $ form_
            [ method_ "POST"
            , action_ $ prefix <> "/file/reconfigure-all"
            ]
        $ input_
            [ type_ "submit"
            , value_ "Reconfigure all files"
            , class_ "btn btn-warning"
            ]

numberFormatH :: Maybe Config -> Html ()
numberFormatH cfg = do
    div_ [class_ "form-check"] $ do
        input_
            $ [ type_ "radio"
              , id_ "number-format1"
              , name_ "number-format"
              , class_ "form-check-input"
              , value_ "american"
              ]
                <> [ term "checked" "checked"
                   | (numberFormat <$> cfg) == Just American
                   ]
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
                <> [ term "checked" "checked"
                   | (numberFormat <$> cfg) == Just European
                   ]
        label_
            [for_ "number-format2", class_ "form-check-label"]
            "European number format"

configure :: Text -> Text -> [Text] -> Maybe Config -> Html ()
configure prefix filename header cfg = do
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
                        "Date field"
                    selectHeader Date header cfg
                div_ [class_ "mb-3 col"] $ do
                    label_
                        [for_ "amount-name", class_ "form-label"]
                        "Amount field"
                    selectHeader Amount header cfg

            div_ [class_ "mb-3 row"] $ do
                div_ [class_ "mb-3 col"] $ do
                    numberFormatH cfg
            div_ [class_ "mb-3 row"] $ do
                label_
                    [for_ "checkbox", class_ "form-check-label"]
                    "Propagate configuration"
                input_
                    [ type_ "checkbox"
                    , name_ "propagate"
                    , checked_
                    , class_ "form-check-input"
                    , required_ ""
                    ]
            div_ [class_ "mb-3"] $ do
                input_
                    [ type_ "submit"
                    , value_ "Submit"
                    , class_ "btn btn-primary"
                    , required_ ""
                    ]

data FieldConfig = Date | Amount

selectHeader :: FieldConfig -> [Text] -> Maybe Config -> Html ()
selectHeader field header cfg = do
    let name = case field of
            Date -> "date-name"
            Amount -> "amount-name"
        oldName = case field of
            Date -> dateField
            Amount -> amountField
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
                    do
                        [value_ h]
                            <> [ term "selected" "selected"
                               | Just h == (oldName <$> cfg)
                               ]
                    $ toHtml h
