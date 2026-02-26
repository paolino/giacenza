{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pages.Types where

import Control.Lens.TH
import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Media ((//), (/:))
import Protolude
import Servant (Accept (..), MimeRender (..))
import Types (Analysis, FileName, NumberFormatKnown)

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
    = ListFiles [(FileName, [Text], Analysis)]
    | AddFile
    deriving (Eq)

makePrisms ''Page
