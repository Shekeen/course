module Network.Server.Handle.Loop where

import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..))
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef)
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, IOException, Exception)
import Control.Monad(forever)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))

import Network.Server.Handle.Accept
import Network.Server.Handle.HandleLens
import Network.Server.Handle.Lens
import Network.Server.Handle.Env
import qualified Data.Set as S

data Loop v f a =
  Loop (Env v -> f a)

type IOLoop v a =
  Loop v IO a

instance Functor f => Functor (Loop v f) where
  fmap f (Loop k) =
    Loop (fmap f . k)

instance Monad f => Monad (Loop v f) where
  return =
    Loop . return . return
  Loop k >>= f =
    Loop (\v -> k v >>= \a ->
      let Loop l = f a
      in l v)

instance MonadTrans (Loop v) where
  lift =
    Loop . const

instance MonadIO f => MonadIO (Loop v f) where
  liftIO =
    lift . liftIO

xprint ::
  IOException
  -> IOLoop v ()
xprint =
  liftIO . print

etry ::
  Exception e =>
  (Env v -> IO a)
  -> Loop v IO (Either e a)
etry k =
  Loop $ try . k

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v () -- per-client
  -> IO a
server i r (Loop f) =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (f (Env q c x))
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

perClient ::
  IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v a) -- read line from client
  -> IOLoop v ()
perClient q f =
  let lp = do k <- etry lGetLine
              case k of Left e -> xprint e
                        Right [] -> lp
                        Right l -> f l >> lp
  in do _ <- q
        lp

loop ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v w) -- read line from client
  -> IO a
loop i r q f =
  server i r (perClient q f)

iorefServer ::
  v -- server initialise
  -> IOLoop v () -- per-client
  -> IO a
iorefServer x =
  server (newIORef x) readIORef

iorefLoop ::
  v -- server initialise
  -> IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v w) -- read line from client
  -> IO a
iorefLoop x q f =
  iorefServer x (perClient q f)

atomicModifyIORef_ ::
  IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef_ r f =
  atomicModifyIORef r (\a -> (f a, ()))