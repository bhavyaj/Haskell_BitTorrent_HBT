module Main(
  tracker,
  hash,
  main
)
where


import Data.Int
import System.IO as S
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import Control.Monad
import Text.Parsec.ByteString
import Text.Parsec

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy as L

tracker = B.empty
hash = B.empty

-- data type used in parsing of .torrent files (meta info files)
data BenCode = BenDict [(BenCode, BenCode)]
            | BenList [BenCode]
            | BenNumber Int64
            | BenStr B.ByteString
              deriving Show

data TorrentInfo = TorrentInfo BenCode
               deriving Show

-- data types of nodes created in parser
benList :: Parser BenCode
benDict ::Parser BenCode
benNumber :: Parser BenCode
benStr :: Parser BenCode
benCode :: Parser BenCode

-- declaration and defination of nodes created in parser
benDict = char 'd' >> fmap BenDict (liftM2 (,) benCode benCode `manyTill` char 'e') 

benList = char 'l' >> fmap BenList (benCode `manyTill` char 'e')

benNumber = char 'i' >> fmap (BenNumber . read) (anyChar `manyTill` char 'e')

benStr = do n <- fmap read (many1 digit)
            char ':'
            xs <- replicateM n anyChar
            return (BenStr (B.pack xs))
 
benCode = benDict <|> benStr <|> benNumber <|> benList

-- parserTorrent will parse the .torrent file contents
parseTorrent :: B.ByteString -> Either ParseError TorrentInfo
parseTorrent torrentData = fmap TorrentInfo (parse benCode "filename" torrentData)

readTorrent handle = fmap parseTorrent (B.hGetContents handle)

-- main function
main = do
	args <- getArgs
	startHBT args

-- initialises the hbt
startHBT :: ([String]) -> IO ()
startHBT (["-h"]) = S.putStrLn "Usage: ./hbt [-h-v] filename"
startHBT (["-a"]) = S.putStrLn "\tAuthors are:\n\t\tBhavya Juneja\n\t\tChandra Prakash\n\t\tHari Shanker\n\t\tPranjal Garg\n\t\tRohit Kumar"
startHBT (["-v"]) = S.putStrLn "This is Haskell Bit Torrent version 0.1.0.0" 
startHBT ([files]) = do
    case files of
           []  -> S.putStrLn "No torrentfile input"
           fileList -> mainProcess fileList


getPiecesStr:: [(BenCode,BenCode)] -> B.ByteString
getPiecesStr ((BenStr a, BenStr b):xs) = 
		case (a==B.pack("pieces")) of
			True -> b
			False -> getPiecesStr xs


-- used to get tracker address from meta info file
getAnnounceStr:: [(BenCode,BenCode)] -> IO()
getAnnounceStr ((BenStr a, BenStr b):xs) = 
		case (a==B.pack("announce")) of
			True -> print $ b
			False -> getAnnounceStr xs

getAnnounceStr (x:xs) = getAnnounceStr xs

-- reads contents from file given in args            
mainProcess fileList = do 
	torrent <- withBinaryFile fileList ReadMode readTorrent        
	print $ torrent	
	--case torrent of
	--	Left _ -> S.putStrLn "Error in torrent file"
	--	Right (TorrentInfo (BenDict b)) -> getAnnounceStr b

