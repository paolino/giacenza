module Pages.Form where

import Lucid
    ( Html
    , Term (term)
    , action_
    , class_
    , div_
    , enctype_
    , for_
    , form_
    , h3_
    , id_
    , input_
    , label_
    , method_
    , name_
    , novalidate_
    , required_
    , type_
    , value_
    )
import Protolude hiding (for_)
import Types
    ( NumberFormatKnown (..)
    )

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

formH :: Text -> Maybe Text -> Maybe Text -> NumberFormatKnown -> Html ()
formH prefix mDateField mAmountField nf = div_ [] $ do
    let dateField' = fromMaybe "Date" mDateField
    let amountField' = fromMaybe "Amount" mAmountField
    h3_ [class_ "mb-3 border-bottom"] "New request"
    form_
        [ method_ "POST"
        , action_ $ prefix <> "/deposit/form"
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
                    numberFormatH nf
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
