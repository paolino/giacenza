{-# LANGUAGE ImportQualifiedPost #-}

module Interactions.SSE where

import Control.Concurrent.STM
    ( TChan
    , newTChanIO
    , readTChan
    , writeTChan
    )
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Interactions.Language (pageH)
import Lucid (Attribute, Html, div_, renderBS, term, toHtml)
import Network.HTTP.Media qualified as M
import Pages.Types (HTML, RawHtml (..))
import Protolude
import Servant
    ( Accept (contentType)
    , Get
    , MimeRender (..)
    , NoFraming
    , Server
    , SourceIO
    , StreamGet
    , type (:<|>) (..)
    , type (:>)
    )
import Servant.Types.SourceT (SourceT (..), StepT (..))

-- imitate the Servant JSON and OctetStream implementations
data EventStream deriving (Typeable)

instance Accept EventStream where
    contentType _ = "text" M.// "event-stream"

instance ToSSE a => MimeRender EventStream a where
    mimeRender _ = toSSE

-- imitate the ToJSON type class
class ToSSE a where
    toSSE :: a -> BL.ByteString

data ChannelledSSE a = ChannelledSSE Text a

instance ToSSE RawHtml where
    toSSE (RawHtml x) = x

instance ToSSE a => ToSSE (ChannelledSSE a) where
    toSSE (ChannelledSSE c x) =
        BL.unlines
            [ "event:" <> toS (encodeUtf8 c)
            , "data:" <> toSSE x <> "\n"
            ]

-- my simple SSE server
type SSE =
    "changes"
        :> StreamGet NoFraming EventStream (SourceIO (ChannelledSSE RawHtml))
        :<|> "sse-page" :> Get '[HTML] RawHtml

sse :: Text -> Html () -> Html ()
sse x =
    div_
        [ term "hx-ext" "sse"
        , term "sse-connect" x
        ]

sseSwap :: Text -> Attribute
sseSwap = term "sse-swap"

sseS :: Server SSE
sseS =
    do
        chan <- liftIO newTChanIO
        liftIO $ async >=> link $ counter chan
        liftIO $ link<=< async $ clock chan
        pure $ sourceFromTChan chan
        :<|> do
            pure $ RawHtml $ renderBS $ pageH $ do
                sse "/changes" $ do
                    div_ [sseSwap "counter"] $ pure ()
                    div_ [sseSwap "clock"] $ pure ()

counter :: TChan (ChannelledSSE RawHtml) -> IO ()
counter chan = loop 0
  where
    loop n = do
        threadDelay 10000
        atomically
            $ writeTChan chan
            $ ChannelledSSE "counter"
            $ RawHtml
            $ renderBS
            $ div_ []
            $ toHtml @Text
            $ show @Int n
        loop $ n + 1

clock :: TChan (ChannelledSSE RawHtml) -> IO ()
clock chan = loop
  where
    loop = do
        threadDelay 1000000
        t <- getCurrentTime
        atomically
            $ writeTChan chan
            $ ChannelledSSE "clock"
            $ RawHtml
            $ renderBS
            $ div_ []
            $ toHtml
            $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
        loop

sourceFromTChan :: TChan a -> SourceT IO a
sourceFromTChan chan = SourceT $ flip ($) loop
  where
    loop = Effect $ do
        x <- atomically $ readTChan chan
        pure $ Yield x loop
