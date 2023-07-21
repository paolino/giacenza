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
import qualified Servant.Types.SourceT as S
import Streaming (Of ((:>)))
import Streaming.ByteString (ByteStream)
import Streaming.ByteString.Char8 (fromChunks)
import Streaming.Internal (Stream (..))

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

-- instance StreamToSourceIO (SafeT IO) where
--   streamToSourceIO ma =
--     S.SourceT $ \k ->
--       runSafeT $
--         liftBaseWith $ \runSafe -> k (consumeStream runSafe ma)

-- consumeStream :: Functor f => (f (StepT m a) -> m (StepT m a)) -> Stream (Of a) f () -> StepT m a
-- consumeStream _ (Return ()) = S.Stop
-- consumeStream runSafe (Effect p) = S.Effect $ runSafe $ fmap (consumeStream runSafe) p
-- consumeStream runSafe (Step (b :> n)) = S.Yield b (consumeStream runSafe n)

instance (StreamToSourceIO m) => ToSourceIO b (Stream (Of b) m ()) where
    toSourceIO = streamToSourceIO

instance (MonadIO m) => FromSourceIO b (Stream (Of b) m ()) where
    fromSourceIO (S.SourceT src) = Effect $ liftIO $ src (pure . toStreaming)

toStreaming :: (MonadIO m) => S.StepT IO b -> Stream (Of b) m ()
toStreaming S.Stop = Return ()
toStreaming (S.Error err) = Effect (liftIO $ fail err)
toStreaming (S.Skip s) = toStreaming s -- drives
toStreaming (S.Effect ms) = Effect (liftIO $ fmap toStreaming ms)
toStreaming (S.Yield x s) = Step (x :> toStreaming s)

instance (MonadIO m) => FromSourceIO ByteString (ByteStream m ()) where
    -- fromSourceIO :: MonadIO m => SourceIO ByteString -> IO (ByteStream m ())
    fromSourceIO (S.SourceT src) = fromChunks $ Effect $ liftIO $ src (return . toStreaming)

-- instance (MonadIO m, a' ~ X, a ~ (), b' ~ (), r ~ ())
--     => FromSourceIO b (Proxy a' a b' b m r)
--   where
--     fromSourceIO src = M $ liftIO $ S.unSourceT src (return . go) where
--         go :: S.StepT IO b -> Proxy X () () b m ()
--         go S.Stop        = Pure ()
--         go (S.Error err) = M (liftIO (fail err))
--         go (S.Skip s)    = go s -- drives
--         go (S.Effect ms) = M (liftIO (fmap go ms))
--         go (S.Yield x s) = Respond x (const (go s))
--     {-# SPECIALIZE INLINE fromSourceIO :: SourceIO x -> Proxy X () () x IO () #-}

-- instance MonadIO m => FromSourceIO a (ListT m a) where
--     fromSourceIO = Select . fromSourceIO
