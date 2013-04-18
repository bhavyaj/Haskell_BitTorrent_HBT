module Listen
    ( startListen
      eventLoop
    )
where

import Control.Concurrent
import Control.Concurrent.STM

import Data.Word

import Network hiding (accept)
import Network.Socket
import Network.BSD


startListen :: Word16 -> Socket
startListen port = liftIO $ do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (toEnum $ fromIntegral port) iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )

eventLoop :: Socket -> Process CF () ()
eventLoop sockFd = do
    c <- asks peerMgrCh
    liftIO $ do
        conn <- accept sockFd
        atomically $ writeTChan c (NewIncoming conn)
    eventLoop sockFd

