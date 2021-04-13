module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Url
import Array
import Dict exposing (Dict)
import Http
import Json.Encode exposing (..)
import Stat exposing (..)
import Global exposing (..)
import List

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
        Unauthorized  baseUrl params -> ( Model (Global.Global key url "" params "" (TopTrackResponse []) (Unauthorized baseUrl params) [] "") [], Nav.load
            ("https://accounts.spotify.com/authorize"
            ++ "?client_id=c6494c8623bc4dde928588fc20354bd4" -- consider not doing this
            ++ "&redirect_uri=" ++ (getRedirectUrl baseUrl)  -- http%3A%2F%2Flocalhost%3A8000%2Fsrc%2FMain.elm" -- may be smarter to have a specific endpoint 
            ++ "&scope=user-top-read,playlist-modify-public,user-library-read" -- this should change based on what we need, maybe user input?
            ++ "&response_type=token") )

        StatDisplay baseUrl params -> 
            let
                token = Maybe.withDefault "" (Dict.get "access_token" params)
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (StatDisplay baseUrl params) [] "") [], Http.request { 
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
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (PlaylistEdit baseUrl params) [] "") [], getNextAlbums token 0)

getNextAlbums : String -> Int -> Cmd Msg
getNextAlbums token curAlbums =
    Http.request { 
                  method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "https://api.spotify.com/v1/me/albums?limit=50&offset=" ++ (String.fromInt curAlbums) -- Request All User albums (Continue until we load all)
                , body = Http.emptyBody
                , expect = Http.expectJson GetLibraryAlbums topAlbumsResponseDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

createNewPlaylist : String -> String -> String -> Cmd Msg
createNewPlaylist token userid playlistName =
    Http.request { 
                  method = "POST"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "https://api.spotify.com/v1/users/" ++ userid ++ "/playlists"
                , body =  Http.jsonBody <| Json.Encode.object [ ("name", Json.Encode.string playlistName ), ("description", Json.Encode.string "test" ), ("public", Json.Encode.string "true") ]
                , expect = Http.expectJson CreatePlaylist playlistDecoder
                , timeout = Nothing
                , tracker = Nothing
                }


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
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
    
        GetLibraryAlbums res -> 
            case res of
                Ok albumResponse -> 
                    let 
                        albums = List.map (\a -> a.album) albumResponse.items
                        continue = albumResponse.next /= "none"
                        newAlbums = List.concat [global.savedAlbums, albums]
                        totalAlbums = List.length newAlbums
                    in
                        ( { model | global = { global | savedAlbums = newAlbums } } ,if continue then getNextAlbums global.auth totalAlbums else Cmd.none)
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage } , Cmd.none ) 
        
        CreatePlaylist res -> 
            case res of
                Ok playlist -> ( { model | global = { global | playlistId = playlist.id } }, Cmd.none)
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage } , Cmd.none )
        
        SendSpotifyRequest req -> 
            case req of RequestCreatePlaylist ->  ( model, createNewPlaylist global.auth "steamymeme" "TEST_PLAYLIST")

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
                    , p [] [ text ("Count of albums: " ++ String.fromInt (List.length global.savedAlbums))]
                    , p [] [ text ("Playlist id: " ++ global.playlistId)]
                    , button [ onClick (SendSpotifyRequest RequestCreatePlaylist)  ] [ text "Create New Playlist" ]
                    , p [] [ text "Error: ", b [] [ text global.errMsg] ]
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

-- Used in init to pass the current url to spotify redirect api --
urlEncode : String -> String
urlEncode c = 
    case c of
       "/" -> "%2F"
       ":" -> "%3A"
       _ -> c

getRedirectUrl : String -> String
getRedirectUrl baseUrl = 
    let 
        split = String.split "" baseUrl
        strArray = List.map urlEncode split
    in 
        List.foldr (++) "" strArray
--------------------------------------------

getEndpoint : String -> String
getEndpoint s =
    let
        temp = String.split "/" s
        filtered = Array.fromList (List.filter (\x -> x /= "") temp)
    in 
        Maybe.withDefault "" (Array.get (Array.length filtered - 1) filtered)

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
