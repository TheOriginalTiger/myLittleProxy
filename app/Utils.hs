module Utils where 

import Data.List 
import Network.DNS
import qualified Data.ByteString.Char8 as C
import Data.List.Split as Split 
import Data.IP



urlToDNS :: Maybe String-> IO(Either DNSError [IPv4])
urlToDNS Nothing = return $ Left FormatError  
urlToDNS (Just furl) = do 
                    let u = case findString "//" furl of 
                              Nothing -> furl 
                              Just _ -> ("www." ++ (concat $ tail $ Split.splitOn "//" furl))
                              where
                                findString search str = findIndex (isPrefixOf search) (tails str)
                    let host = C.pack u
                    rs <- makeResolvSeed defaultResolvConf
                    withResolver rs $ \resolver -> lookupA resolver host
fromDNSToInt :: [IPv4]->Int
fromDNSToInt addr = foldr (+) 0 (fromIPv4 $ head addr)