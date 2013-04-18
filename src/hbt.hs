import Data.Int
import System.IO
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.Word
import System.Environment
import System.Exit
-- import Data.ByteString.UTF8 as U8

import Control.Monad
import Text.Parsec.ByteString
import Text.Parsec

data BValue = BDictionary [(BValue, BValue)]
            | BList [BValue]
            | BNumber Int64
            | BString B.ByteString
              deriving Show

data Torrent = Torrent BValue
               deriving Show


emptyBS = B.empty

bList :: Parser BValue
bDictionary ::Parser BValue
bNumber :: Parser BValue
bString :: Parser BValue
bValue :: Parser BValue

bDictionary = char 'd' >> fmap BDictionary (liftM2 (,) bValue bValue `manyTill` char 'e') 

bList = char 'l' >> fmap BList (bValue `manyTill` char 'e')

bNumber = char 'i' >> fmap (BNumber . read) (anyChar `manyTill` char 'e')

bString = do n <- fmap read (many1 digit)
             char ':'
             xs <- replicateM n anyChar
             return (BString (B.pack xs))


bValue = bDictionary <|> bString <|> bNumber <|> bList

parseTorrent :: B.ByteString -> Either ParseError Torrent
parseTorrent torrentData = fmap Torrent (parse bValue "filename" torrentData)

readTorrent handle = fmap parseTorrent (B.hGetContents handle)

main = do
	args <- getArgs
	startHBT args

startHBT :: ([String]) -> IO ()
startHBT (["-h"]) = putStrLn "Usage: ./hbt [-h-v] filename"
startHBT (["-a"]) = putStrLn "\tAuthors are:\n\t\tBhavya Juneja\n\t\tChandra Prakash\n\t\tHari Shanker\n\t\tPranjal\n\t\tRohit Kumar"
startHBT (["-v"]) = putStrLn "This is Haskell Bit Torrent versio 0.1" 
startHBT ([files]) = do
    case files of
           []  -> putStrLn "No torrentfile input"
           fileList -> mainProcess fileList

mainProcess fileList = do 
	torrent <- withBinaryFile fileList ReadMode readTorrent
        print $ torrent
