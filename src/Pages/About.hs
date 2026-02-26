module Pages.About where

import Lucid
    ( Html
    , a_
    , h5_
    , href_
    , p_
    )
import Protolude hiding (for_)

-- | Render the about section with a link to the documentation site.
aboutH :: Html ()
aboutH = do
    h5_ "About"
    p_
        "Giacenza computes the average daily deposit (giacenza media) and the \
        \end-of-year balance (saldo) of a bank account from CSV transaction files."
    p_ $ do
        "For detailed usage instructions and a step-by-step tutorial, visit the "
        a_ [href_ "https://paolino.github.io/giacenza/"] "documentation"
        "."
