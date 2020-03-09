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


main = do
  lsock <- socket AF_INET Stream defaultProtocol
  bind lsock (SockAddrInet 3000 0x0100007f)
  listen lsock 1
  handleSocket lsock


handleSocket :: Socket -> IO()
handleSocket lsock = forever $ do
                        (csock, _) <- accept lsock
                        hs <- socketConnection "" 3000 csock
                        req <- receiveHTTP hs
                        case req of
                          Left _ -> handleBadRequests (5,0,0) hs
                          Right r ->  do
                                        putStrLn "got it !"
                                        let newr = replaceHeader HdrUserAgent "User-Agent: ti/Pidor 3.22" r
                                        let url = findHeader HdrHost newr
                                        addr <- urlToDNS url 
                                        eresp <- forwardMessage addr newr
                                        case eresp of 
                                          Left _ -> handleBadRequests (5,0,0) hs
                                          Right resp -> respondHTTP hs resp

                        Network.HTTP.close hs
                    
forwardMessage :: Either DNSError [IPv4] -> Request [Char] -> IO (Result (Response [Char]))
forwardMessage (Left _) _ = return $ Right $ formBResponse (5,0,0)
forwardMessage (Right dnsAdress) newr = bracket cnct dcnct snd where
                                          cnct = do 
                                                  sendingSock <- socket AF_INET Stream defaultProtocol
                                                  connect sendingSock $ SockAddrInet 80 $ toHostAddress $ head dnsAdress 
                                                  handler <- socketConnection "" 2228 sendingSock 
                                                  return (sendingSock, handler)
                                          dcnct (sendingSock, handler) = Network.HTTP.close handler >> close' sendingSock                                
                                          snd (sendingSock, handler) = sendHTTP handler newr 



