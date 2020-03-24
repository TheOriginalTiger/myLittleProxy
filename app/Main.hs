module Main where

import Lib
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Network.HTTP
import Network.Socket
import Network.URI
import Network.DNS
import Data.IP
import ErrorCodes
import Utils
import Network.Stream
import Control.Exception
import Control.Concurrent
import HashTable

main = do
  lsock <- socket AF_INET Stream defaultProtocol
  bind lsock (SockAddrInet 3000 0x0100007f)
  listen lsock 1
  mvar <- newMVar defaultHashTable
  handleSocket lsock mvar


handleSocket :: Socket -> MVar (HashTable Int (Response [Char]))-> IO()
handleSocket lsock mv = forever $ do
                        (csock, _) <- accept lsock
                        hs <- socketConnection "" 3000 csock
                        forkIO $ threadedHandle hs mv


threadedHandle :: HandleStream [Char] -> MVar (HashTable Int (Response [Char])) -> IO()
threadedHandle hs mv = do
          req <- receiveHTTP hs 
          case req of
            Left _ -> handleBadRequests (5,0,0) hs
            Right r ->  do
                          putStrLn "got it !"
                          
                          let newr = replaceHeader HdrUserAgent "User-Agent: ti/Pidor 3.22" r
                          let url = findHeader HdrHost newr
                          addr <- urlToDNS url                          
                          
                  
                          eresp <- preForwardMessage addr newr mv -- ! PRE
                          case eresp of 
                            Left _ -> handleBadRequests (5,0,0) hs
                            Right resp -> respondHTTP hs resp
          Network.HTTP.close hs
-- messageHub
preForwardMessage :: Either DNSError [IPv4] -> Request [Char] -> MVar (HashTable Int (Response [Char])) -> IO (Result (Response [Char]))
preForwardMessage (Left _) _ _ = return $ Right $ formBResponse (5,0,0)
preForwardMessage (Right dnsAdress) newr mv = do 
                                                  hashtable <- takeMVar mv
                                                  let cached = (fromDNSToInt dnsAdress) `at` hashtable 
                                                  putMVar mv hashtable
                                                  case cached of
                                                    Nothing -> do
                                                                putStrLn "not Cached yet"
                                                                res <- forwardMessage dnsAdress newr
                                                                case res of 
                                                                  Right result -> do 
                                                                                    ht <- takeMVar mv 
                                                                                    let newht = insertToTable ht (fromDNSToInt dnsAdress) result
                                                                                    putMVar mv newht 
                                                                return $ res 
                                                    Just res -> do
                                                                  putStrLn "now cashed!"
                                                                  return $ Right res  




forwardMessage :: [IPv4] -> Request [Char]-> IO (Result (Response [Char]))
forwardMessage dnsAdress newr  = bracket cnct dcnct snd where
                                          cnct = do 
                                                  sendingSock <- socket AF_INET Stream defaultProtocol
                                                  connect sendingSock $ SockAddrInet 80 $ toHostAddress $ head dnsAdress 
                                                  handler <- socketConnection "" 2228 sendingSock 
                                                  return (sendingSock, handler)
                                          dcnct (sendingSock, handler) = Network.HTTP.close handler >> close' sendingSock                                
                                          snd (sendingSock, handler) = sendHTTP handler newr 



