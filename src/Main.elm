module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Array
import Dict exposing (Dict)
import Http
import Json.Decode exposing (..)
import Stat exposing (..)
import Global exposing (..)

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- Model
type alias Model =
  { global : Global.Global
  , stats : List Stat.Stat
  }

updateGlobal : Model -> Global.Global -> Model
updateGlobal model global = { model | global = global }

-- INIT 

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case urlParser url of
        Unauthorized  baseUrl params -> ( Model (Global.Global key url "" params "" (TopTrackResponse []) (Unauthorized baseUrl params) ) [], Nav.load
            ("https://accounts.spotify.com/authorize"
            ++ "?client_id=c6494c8623bc4dde928588fc20354bd4" -- consider not doing this
            ++ "&redirect_uri=http%3A%2F%2Flocalhost%3A8001%2FMain.html" -- http%3A%2F%2Flocalhost%3A8000%2Fsrc%2FMain.elm" -- may be smarter to have a specific endpoint 
            ++ "&scope=user-top-read" -- this should change based on what we need, maybe user input?
            ++ "&response_type=token") )

        StatDisplay baseUrl params -> 
            let
                token = Maybe.withDefault "" (Dict.get "access_token" params)
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (StatDisplay baseUrl params)) [], Http.request { 
                      method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://api.spotify.com/v1/me/top/tracks/?time_range=long_term&limit=50"
                    , body = Http.emptyBody
                    , expect = Http.expectJson GetTracks topTrackResponseDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    } )
    
        PlaylistEdit baseUrl params -> 
            let
                token = Maybe.withDefault "" (Dict.get "access_token" params)
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (PlaylistEdit baseUrl params)) [], Http.request { 
                      method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://api.spotify.com/v1/me/top/tracks/?time_range=long_term&limit=50" -- Request All User albums (Continue until we load all)
                    , body = Http.emptyBody
                    , expect = Http.expectJson GetTracks topTrackResponseDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    } )

-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  let 
    global = model.global 
    stats = model.stats
  in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->  ( model, Nav.pushUrl model.global.key (Url.toString url) )

                Browser.External href -> ( model, Nav.load href )

        UrlChanged url ->
            case urlParser url of
                Unauthorized baseUrl params -> ( updateGlobal model { global |  url = url, params = params, currentRoute = (Unauthorized baseUrl params)}, Cmd.none )

                StatDisplay baseUrl params -> ( updateGlobal model { global |
                        url = url, 
                        params = params, 
                        auth = Maybe.withDefault "" (Dict.get "access_token" params),
                        currentRoute = (StatDisplay baseUrl params)
                    }, Cmd.none )
                
                PlaylistEdit baseUrl params -> ( updateGlobal model { global |
                        url = url, 
                        params = params, 
                        auth = Maybe.withDefault "" (Dict.get "access_token" params),
                        currentRoute = (PlaylistEdit baseUrl params)
                    }, Cmd.none )

        GetTracks res -> 
            case res of
                    Ok topTracks -> ( { model | global = { global | topTracks = topTracks }, stats = Stat.init } , Cmd.none)
                    Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage } , Cmd.none ) 


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
  let 
    global = model.global
    stats = model.stats
  in
  -- MOVE THESE OUT TO DIFFERENT FILES
    case global.currentRoute of  
        StatDisplay _ _ -> { title = "LADIFY"
            , body =
                [ div [ class "main" ]
                    [ Stat.view global stats
                    , p [] 
                    [ b [] [ text "DEBUG INFO: " ]
                    , p [] [ text "The current URL is: " ]
                    , b [] [ text (Url.toString global.url) ]
                    , p [] [ text "The auth key: ", b [] [ text global.auth] ]
                    , p [] [ text "Error: ", b [] [ text global.errMsg] ]
                    ]
                    ]
                ]
            }
        PlaylistEdit _ _ -> { title = "Playlist Edit"
            , body =
                [ div [ class "main" ]
                    [ -- Stat.view global stats
                    p []  [ b [] [ text "PLACEHOLDER ROUTE" ] ]
                    ]
                ]
            }
        Unauthorized _ _ -> { title = "AUTH FAILED"
            , body =
                [ div [ class "main" ]
                    [ p []  
                    [ b [] [ text "AUTHENTICATION FAILED: " ] 
                    , p [] [ text "The current URL is: " ]
                    , b [] [ text (Url.toString global.url) ]
                    , p [] [ text "The auth key: ", b [] [ text global.auth] ]
                    , p [] [ text "Error: ", b [] [ text global.errMsg] ]
                    ]
                    ]
                ]
            }

-- old images code
-- if List.isEmpty model.topTracks.items
-- then text "NOT LOADED"
-- else ul [] (topTracksToImages model.topTracks)

viewLink : String -> Html Msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

-- IDK WHY THIS ISNT WORKING
{-
topAlbumsToImages : TopAlbumsResponse -> List (Html Msg)
topAlbumsToImages res =
    let
        firstArts = List.map (\x -> Maybe.withDefault (AlbumArt "") (List.head x.images) ) res.items 
    in
        List.map (\a -> displayImg a.url) firstArts

topTracksToImages : TopTrackResponse -> List (Html Msg)
topTracksToImages res =
    let
        firstArts = List.map (\x -> Maybe.withDefault (AlbumArt "") (List.head x.album.images) ) res.items 
    in
        List.map (\a -> displayImg a.url) firstArts
-}

splitPair : String -> (String, String)
splitPair s =
    let
        temp = Array.fromList (String.split "=" s)
        key = Maybe.withDefault "" (Array.get 0 temp)
        value = Maybe.withDefault "" (Array.get 1 temp)
    in 
        (key, value)

getEndpoint : String -> String
getEndpoint s =
    let
        temp = Array.fromList (String.split "/" s)
    in 
        Maybe.withDefault "" (Array.get (Array.length temp - 1) temp)

containsAuth : (Dict String String) -> Bool
containsAuth p = Dict.member "access_token" p

urlParser : Url.Url -> Route
urlParser url =
    let
        temp = Array.fromList (String.split "#" (Url.toString url))
        base = Maybe.withDefault "" (Array.get 0 temp)
        params = Maybe.withDefault "" (Array.get 1 temp)
        paramsList =  Dict.fromList ( List.map splitPair (String.split "&" params) )
    in
        if containsAuth paramsList then
            case getEndpoint base of
                "edit" -> PlaylistEdit base paramsList
                _ -> StatDisplay base paramsList
        else
            Unauthorized base paramsList

-- Error handling 
htmlErrorToString : Http.Error -> String
htmlErrorToString error = 
    case error of 
        Http.BadUrl x -> "badurl" ++ x 
        Http.Timeout -> "timout"
        Http.NetworkError -> "networkerror"
        Http.BadStatus x -> "badstatus" ++ String.fromInt x
        Http.BadBody x -> "badbody"++ x
