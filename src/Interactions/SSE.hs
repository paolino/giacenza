{-# LANGUAGE ImportQualifiedPost #-}

module Interactions.SSE where

import Control.Concurrent.STM
    ( TChan
    , TVar
    , modifyTVar
    , newTChanIO
    , readTChan
    , readTVarIO
    , writeTChan
    )
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Interactions.Language (getX, pageH, trigger)
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

newtype ChannelledSSE = ChannelledSSE Text


instance ToSSE ChannelledSSE where
    toSSE (ChannelledSSE c) =
        BL.unlines
            [ "event:" <> toS (encodeUtf8 c)
            , "data:\n"
            ]

-- my simple SSE server
type SSE =
    "changes"
        :> StreamGet NoFraming EventStream (SourceIO ChannelledSSE)
        :<|> "sse-page" :> Get '[HTML] RawHtml
        :<|> "clock" :> Get '[HTML] RawHtml
        :<|> "counter" :> Get '[HTML] RawHtml

sse :: Text -> Html () -> Html ()
sse x =
    div_
        [ term "hx-ext" "sse"
        , term "sse-connect" x
        ]

sseSwap :: Text -> Attribute
sseSwap = term "sse-swap"

sseS :: TVar Int -> Text -> Server SSE
sseS counterVar prefix =
    do
        chan <- liftIO newTChanIO
        liftIO $ async >=> link $ counter counterVar chan
        liftIO $ link <=< async $ clock chan
        pure $ sourceFromTChan chan
        :<|> do
            pure $ RawHtml $ renderBS $ pageH $ do
                sse (prefix <> "/changes") $ replicateM_ 3 do
                    div_ [trigger "sse:counter", getX prefix "counter"] $ pure ()
                    div_ [trigger "sse:clock", getX prefix "clock"] $ pure ()
        :<|> do
            t <- liftIO getCurrentTime
            pure $ RawHtml $ renderBS $ div_ [] $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
        :<|> do
            n <- liftIO $ readTVarIO counterVar
            pure $ RawHtml $ renderBS $ div_ [] $ toHtml @Text $ show n

counter :: TVar Int -> TChan ChannelledSSE  -> IO ()
counter counterV chan = loop
  where
    loop = do
        threadDelay 100000
        atomically $ do
            writeTChan chan
                $ ChannelledSSE "counter"
            modifyTVar counterV (+ 1)
        loop

clock :: TChan ChannelledSSE  -> IO ()
clock chan = loop
  where
    loop = do
        threadDelay 1000000
        atomically
            $ writeTChan chan
            $ ChannelledSSE "clock"
        loop

sourceFromTChan :: TChan a -> SourceT IO a
sourceFromTChan chan = SourceT $ flip ($) loop
  where
    loop = Effect $ do
        x <- atomically $ readTChan chan
        pure $ Yield x loop
