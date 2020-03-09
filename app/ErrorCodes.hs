module ErrorCodes where 
import Network.HTTP 
 
type ErrorCode = (Int,Int,Int)



formBResponse :: ErrorCode -> Response [Char]
formBResponse (5,0,0) = Response (5,0,0) "Internal Server Error" [] ""
formBResponse (4,0,4) = Response (4,0,4) "This site can’t be reached" [] "server IP address could not be found"


handleBadRequests :: ErrorCode -> HandleStream [Char] -> IO()
handleBadRequests code h = respondHTTP h $ formBResponse code