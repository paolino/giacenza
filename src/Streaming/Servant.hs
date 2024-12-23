{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streaming.Servant where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Servant.API.Stream
    ( FromSourceIO (..)
    , SourceIO
    , ToSourceIO (..)
    )
import Servant.Types.SourceT qualified as S
import Streaming (Of ((:>)))
import Streaming.ByteString (ByteStream)
import Streaming.ByteString.Char8 (fromChunks)
import Streaming.Internal (Stream (..))
import Prelude

class StreamToSourceIO m where
    streamToSourceIO :: Stream (Of b) m () -> SourceIO b

instance StreamToSourceIO IO where
    streamToSourceIO :: Stream (Of b) IO () -> SourceIO b
    streamToSourceIO ma = S.SourceT ($ go ma)
      where
        go :: Stream (Of b) IO () -> S.StepT IO b
        go (Return ()) = S.Stop
        go (Effect p) = S.Effect (fmap go p)
        go (Step (b :> n)) = S.Yield b (go n)

instance StreamToSourceIO m => ToSourceIO b (Stream (Of b) m ()) where
    toSourceIO = streamToSourceIO

instance MonadIO m => FromSourceIO b (Stream (Of b) m ()) where
    fromSourceIO (S.SourceT src) = liftIO $ src (pure . toStreaming)

toStreaming :: MonadIO m => S.StepT IO b -> Stream (Of b) m ()
toStreaming S.Stop = Return ()
toStreaming (S.Error err) = Effect (liftIO $ fail err)
toStreaming (S.Skip s) = toStreaming s -- drives
toStreaming (S.Effect ms) = Effect (liftIO $ fmap toStreaming ms)
toStreaming (S.Yield x s) = Step (x :> toStreaming s)

instance MonadIO m => FromSourceIO ByteString (ByteStream m ()) where
    -- fromSourceIO :: MonadIO m => SourceIO ByteString -> IO (ByteStream m ())
    fromSourceIO (S.SourceT src) = fmap fromChunks $ liftIO $ src (pure . toStreaming)
