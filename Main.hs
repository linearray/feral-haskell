{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


--import Turtle
import Network.Curl
--import Network.HTTP
import Data.List
import Text.Regex.TDFA
import GHC.Conc
import Debug.Trace
import Control.Monad (foldM)    

    
data Route =
    Route {
      description :: String,
      nexthop     :: String,
      testfile    :: String
    }

routes =
    [
      Route "Default" "0.0.0.0" "https://feral.io/test.bin",
      Route "Cogent" "130.117.255.36" "https://cogent-1.feral.io/test.bin",
      Route "Fiber Ring #1" "87.255.32.229" "https://fr-1.feral.io/test.bin",
      Route "Fiber Ring #2" "87.255.32.249" "https://fr-2.feral.io/test.bin",
      Route "GTT" "77.67.64.81" "https://gtt-1.feral.io/test.bin",
      Route "Level3" "213.19.196.233" "https://level3.feral.io/test.bin",
      Route "NTT #1" "81.20.64.101" "https://ntt-1.feral.io/test.bin",
      Route "NTT #2" "81.20.69.197" "https://ntt-2.feral.io/test.bin"
    ]

reroutePage = "https://network.feral.io/reroute"

lookingGlassHost :: String -> String
lookingGlassHost ip =
    "https://network.feral.io/looking-glass?action=traceroute&host="
      ++ ip

curlDLFlags = [CurlTimeout 10, CurlFollowLocation True]
            
download :: URLString -> IO InfoValue
download f = withCurlDo $ do
             (resp :: CurlResponse) <- curlGetResponse_ f curlDLFlags
             respGetInfo resp $ SpeedDownload

-- |Returns the string identifier of the currently selected route
getCurrentRoute :: IO String
getCurrentRoute =
    withCurlDo $ do
      (resp::CurlResponse) <- curlGetResponse_ reroutePage []
      let body = lines $ respBody resp
      let m = head $ filter (isInfixOf "checked") body
      return m

-- |Sets the argument route on the reroute page
setRoute :: Route -> IO ()
setRoute r =
    withCurlDo $ do
      putStrLn $ "Setting next hop to " ++ description r
      let b = "nh=" ++ nexthop r
      curlMultiPost
        reroutePage
        [CurlVerbose False, CurlPostFields [b], CurlWriteFunction ignoreOutput]
        []

-- |Sadly, the list of settable routes is not static. We need to
--  check which routes are currently available on the feral reroute page.
filterRoutes :: [Route] -> IO [Route]
filterRoutes rs = do
  (resp::CurlResponse) <- curlGetResponse_ reroutePage []
  let body = lines $ respBody resp
  let determinedRoutes = filter (\el -> not . null $ filter (isInfixOf (nexthop el)) body) rs

  return $ (head routes):determinedRoutes

-- |Checks the route to the given IP address for a number of times
--  or until the route was successfully set
checkRoute :: String -> Route -> IO Bool
checkRoute myip r =
    putStrLn "Waiting for route change to take effect" >>
    checkOnce myip (nexthop r) 10
        where
          -- We cannot really check whether the Default route is in
          -- effect, so we just wait and be done with it.
          checkOnce _ "0.0.0.0" _ = do
            threadDelay 5000000
            return True
          checkOnce myip hop 0 = return False
          checkOnce myip hop n =
              withCurlDo $ do
                threadDelay 2000000
                (resp::CurlResponse) <- curlGetResponse_ (lookingGlassHost myip) []

                let body = lines $ respBody resp

                if (not . null $ filter (isInfixOf hop) body)
                then return True
                else checkOnce myip hop (n-1)

--  
getCurrentIP :: IO String
getCurrentIP =
    withCurlDo $ do
      (resp::CurlResponse) <- curlGetResponse_ reroutePage []
      let body = lines $ respBody resp
      let ipfoo = head $ filter (isInfixOf "Your IPv4 address is") body

      let patt::String = "[[:digit:]]{1,3}\\.[[:digit:]]{1,3}\\.[[:digit:]]{1,3}\\.[[:digit:]]{1,3}"

      let m = (ipfoo =~ patt :: String)

      return m


main :: IO ()
main = do
  availRoutes <- filterRoutes (tail routes)
  
  bestr <- foldM (\os@(s,_) r -> do
    ip <- getCurrentIP
    setRoute r
    routeSuccess <- checkRoute ip r
    if not routeSuccess
    then do
        putStrLn $ "Cannot test " ++ (description r) ++ ". Moving on.\n"
        return os
    else do
        putStrLn "Starting download."
        ns <- download (testfile r)
        let nss = show ns
        let nsi = round $ (read nss :: Double)
        putStrLn $ "Speed achieved (Bytes per second): " ++ show nsi ++ "\n"

        if nsi > s
        then return (nsi, r)
        else return os) (0,head availRoutes) availRoutes

  --print $ fst bestr
  putStrLn $ "\n\nBest result: " ++ description (snd bestr)
  putStrLn . show $ fst bestr


  setRoute $ snd bestr
