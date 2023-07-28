module Pages.AddFile where

import Lucid
    ( Html
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

addFileH :: Text -> Html ()
addFileH prefix = div_ [] $ do
    h3_ [class_ "mb-3 border-bottom"] "Add file"
    form_
        [ method_ "POST"
        , action_ $ prefix <> "/file"
        , enctype_ "multipart/form-data"
        , class_ "needs-validation"
        , novalidate_ ""
        ]
        $ do
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
