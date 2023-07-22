module Pages.About where

import Lucid
    ( Html
    , h4_
    , li_
    , p_
    , ul_
    )
import Protolude hiding (for_)

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
        li_
            "The amount field must be a number, with the decimal separator chosen in the form."

    p_
        "The result is a table with the average deposit and the balance for each year."
