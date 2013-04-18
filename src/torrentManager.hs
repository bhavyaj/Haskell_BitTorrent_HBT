module TorrentManager(
	startTorrent
)
where

import Network.HTTP
import Network.HTTP.Base
import System.Cmd
import System.Process

import hbt as H

data 

startTorrent hash = do
	print $ urlEncode hash
	rsp <- Network.HTTP.simpleHTTP (getRequest $ tracker ++ urlEncode hash)
	fmap (take 100) (getResponseBody rsp)
	output <- openFile "peerInfo.txt" WriteMode
	hPrint output rsp >>= parseResponse  >>= peerConnect	
	

parseResponse fileList = 
	peerInfo <- withBinaryFile fileList ReadMode readTorrent
	getPeerIp peerInfo

-- getPeerIp :: To be defined
-- processCreate :: To be defined

peerConnect IpList = 
	map IpList startPeer

startPeer ip = 
   lookChannel <- liftIO newTChanIO
   statChannel <- liftIO $ newTChanIO
   waitChannel <- liftIO $ newEmptyTMVarIO
   supervChannel <- liftIO newTChanIO
   pmChannel <- liftIO $ newTChanIO
   rShared <- atomically $ newTVar []
   sShared <- atomically $ newTVar []
   (tid, _) <- processCreate "Worker"
 	 (workersWatch ++		
	   [ Worker $ start lookChannel statChannel sShared chokeChannel pid pmChannel
	  , Worker $ Listen.start pmChannel
	   ])
   return ()

