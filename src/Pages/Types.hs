module Pages.Types where

import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Media ((//), (/:))
import Protolude
import Servant (Accept (..), MimeRender (..))
import Types (NumberFormatKnown, Result)

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

data Feedback = Feedback
    { dateField :: Text
    , amountField :: Text
    , numberFormat :: NumberFormatKnown
    , clientFilename :: Text
    }
    deriving (Eq, Show)

data Page
    = Home
    | About
    | Positive Feedback Result
    | Negative Feedback Text
    deriving (Eq)
