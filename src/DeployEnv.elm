module DeployEnv exposing (serverUrl)

{-| Backend server URL
-}


url : String
url =
    "https://192.168.1.217:8008"


routePrefix : String
routePrefix =
    "/api/v1/"


serverUrl : String
serverUrl =
    url ++ routePrefix
