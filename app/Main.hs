module Main where


import Lib
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Network.HTTP
import Network.Socket
import Network.URI
import Network.DNS
import Data.IP
import Data.List 
import Data.List.Split as Split 



parseUrl :: Maybe String-> IO(Either DNSError [IPv4])
parseUrl Nothing = return $ Left FormatError  
parseUrl (Just furl) = do 
                    let u = case findString "//" furl of 
                              Nothing -> furl 
                              Just _ -> ("www." ++ (concat $ tail $ Split.splitOn "//" furl))
                              where
                                findString search str = findIndex (isPrefixOf search) (tails str)     
                       
                    
                    let host = C.pack u
                    rs <- makeResolvSeed defaultResolvConf
                    a <- withResolver rs $ \resolver -> lookupA resolver host
                    return a -- :D ugly vkoryachka 

main = do
  lsock <- socket AF_INET Stream defaultProtocol
  bind lsock (SockAddrInet 3000 0x0100007f)
  listen lsock 1
  forever $ do
    (csock, _) <- accept lsock
    hs <- socketConnection "" 3000 csock
    req <- receiveHTTP hs
    case req of
      Left _ -> respondHTTP hs $ (Response (5,0,0) "Internal Server Error" [] "")
      Right r ->  do
                    putStrLn "got it !"
                    let newr = replaceHeader HdrUserAgent "User-Agent: ti/Pidor" r
                    let url = findHeader HdrHost newr
                    addr <- parseUrl url 
                    case addr of 
                      Left _ -> respondHTTP hs $ (Response (4,0,4) "This site can’t be reached" [] "server IP address could not be found")
                      Right h -> do                                   
                                  respondHTTP hs $ (Response (2,0,0) "OK" [] "Hello HTTP")

               
                    
                    Network.HTTP.close hs

