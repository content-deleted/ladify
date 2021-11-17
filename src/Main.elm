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
import Platform exposing (Task)
import Task
import Process
import Json.Decode exposing (float)
import Markdown exposing (defaultOptions)

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
        Unauthorized  baseUrl params -> ( Model (Global.Global key url "" params "" (TopTrackResponse []) (Unauthorized baseUrl params) [] "" Global.newUser Global.newDataSources []) [], Nav.load
            ("https://accounts.spotify.com/authorize"
            ++ "?client_id=c6494c8623bc4dde928588fc20354bd4" -- consider not doing this
            ++ "&redirect_uri=" ++ (getRedirectUrl baseUrl)  -- http%3A%2F%2Flocalhost%3A8000%2Fsrc%2FMain.elm" -- may be smarter to have a specific endpoint 
            ++ "&scope=user-top-read,playlist-modify-public,user-library-read" -- this should change based on what we need, maybe user input?
            ++ "&response_type=token") )

        StatDisplay baseUrl params -> 
            let
                token = Maybe.withDefault "" (Dict.get "access_token" params)
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (StatDisplay baseUrl params) [] "" Global.newUser Global.newDataSources []) [], Http.request { 
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
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (PlaylistEdit baseUrl params) [] "" Global.newUser Global.newDataSources []) [], Cmd.batch [(getNextAlbums token 0), (getUserInfo token)])
        
        PlaylistInfo baseUrl params -> 
            let
                token = Maybe.withDefault "" (Dict.get "access_token" params)
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse []) (PlaylistInfo baseUrl params) [] "" Global.newUser Global.newDataSources []) [], Cmd.batch [(getNextPlaylists token 0), (getUserInfo token)])

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

getNextPlaylists : String -> Int -> Cmd Msg
getNextPlaylists token curPlaylists =
    Http.request { 
                  method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "https://api.spotify.com/v1/me/playlists?limit=50&offset=" ++ (String.fromInt curPlaylists) -- Request All User albums (Continue until we load all)
                , body = Http.emptyBody
                , expect = Http.expectJson GetUserPlaylists userPlaylistsResponseDecoder
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

addSongsToPlaylist : String -> String -> Array.Array String  -> Cmd Msg
addSongsToPlaylist token playlistId songsIdList =
    Http.request {
                  method = "POST"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "https://api.spotify.com/v1/playlists/" ++ playlistId ++ "/tracks"
                , body =  Http.jsonBody <| Json.Encode.object [ ("uris", Json.Encode.array Json.Encode.string songsIdList ) ]
                , expect = Http.expectWhatever (ProcessAddSongsToPlaylist (SendSpotifyRequest (RequestAddSongsPlaylist songsIdList)))
                , timeout = Nothing
                , tracker = Nothing
                }

getUserInfo : String -> Cmd Msg
getUserInfo token =
    Http.request {
                  method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                , url = "https://api.spotify.com/v1/me/"
                , body = Http.emptyBody
                , expect = Http.expectJson GetUser userDecoder
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

                PlaylistInfo baseUrl params -> ( updateGlobal model { global |
                        url = url, 
                        params = params, 
                        auth = Maybe.withDefault "" (Dict.get "access_token" params),
                        currentRoute = (PlaylistInfo baseUrl params)
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
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage }, Cmd.none)
        
        CreatePlaylist res -> 
            case res of
                Ok playlist -> 
                 let
                     updatedModel =  { model | global = { global | playlistId = playlist.id } }
                 in
                    ( updatedModel, Cmd.batch (generateSongAddRequests updatedModel))

                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage }, Cmd.none )

        ProcessAddSongsToPlaylist req res ->
            case res of
                Ok _ -> ( model, Cmd.none)
                Err errorMessage -> ( 
                    updateGlobal model { global | errMsg = (htmlErrorToString errorMessage) ++ "retrying" }, 
                    case errorMessage of
                        Http.BadStatus x -> 
                            case x of -- This logic retries if we have an error, its evil
                                500 -> (delay 100 req) -- this is a genuine server error we sent like 100 requests
                                429 -> (delay 10000 req)-- spotify is being a whore lets wait a minute and try again
                                _ -> Cmd.none -- idk give up
                        _ -> Cmd.none )
        
        SendSpotifyRequest req ->
            case req of 
                RequestCreatePlaylist ->  ( model, createNewPlaylist global.auth global.currentUser.id "Full Library")
                RequestAddSongsPlaylist tracks -> ( model, addSongsToPlaylist model.global.auth model.global.playlistId tracks)
        
        GetUser res -> 
            case res of
                Ok user -> ( { model | global = { global | currentUser = user } }, Cmd.none)
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage }, Cmd.none)

        -- This needs to write the playlist to a dictionary of playlists
        -- The dic should be String : { Tracks | Null }
        TogglePlaylistStat playlistId -> 
            let 
                newPlaylists = List.map (\p -> if p.playlist.id == playlistId then {p | enabled = not p.enabled} else p) global.savedPlaylists
            in
                ( { model | global = { global | savedPlaylists = newPlaylists } } , Cmd.none)

        GetUserPlaylists res -> 
            case res of
                Ok playlistsResponse -> 
                    let 
                        userPlaylists = List.map (\p -> { playlist = p, enabled = False, tracks = []}) playlistsResponse.items
                        continue = playlistsResponse.next /= "none"
                        newPlaylists = List.concat [global.savedPlaylists, userPlaylists]
                        totalPlaylists = List.length newPlaylists
                    in
                        ( { model | global = { global | savedPlaylists = newPlaylists } } ,if continue then getNextPlaylists global.auth totalPlaylists else Cmd.none)
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage }, Cmd.none)


delay : Float -> Msg -> Cmd Msg
delay time msg =
  Process.sleep time
  |> Task.perform (\_ -> msg)

generateSongAddRequests : Model -> List (Cmd Msg)
generateSongAddRequests model =
  let
    allTracks = List.concatMap (\album -> album.tracks.items) model.global.savedAlbums
    trackIds = Array.fromList (List.map (\t -> "spotify:track:"++t.id) allTracks)
  in
    requestsFromSongs trackIds model []

requestsFromSongs : Array.Array String -> Model -> List (Cmd Msg) -> List (Cmd Msg)
requestsFromSongs remainingTracks model requests =
  let count = Array.length remainingTracks
  in 
    if count >= 100 then requestsFromSongs (Array.slice 99 (Array.length remainingTracks) remainingTracks) model ( (addSongsToPlaylist model.global.auth model.global.playlistId (Array.slice 0 100 remainingTracks)) :: requests)
    else ( (addSongsToPlaylist model.global.auth model.global.playlistId remainingTracks) :: requests)

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
                [ div [ class "edit" ]
                    [ -- Stat.view global stats
                    div [ class "center-panel" ]
                        [
                        b [class "edit-title" ] [ text "Library Playlist Generator" ]
                        , div [ class "edit-description" ] 
                            [ p [] [ text "This tool was created to allow users to shuffle their entire library from a single source. Currently it will automatically pull all of a user saved albums and add them to one playlist, but support for consolidation of liked songs and user playlists is planned for a future release." ]
                            ]
                        , div [ class "playlist-embed"] [ displayPlaylist global.playlistId ]
                        , if global.currentUser.id /= "" then button [ onClick (SendSpotifyRequest RequestCreatePlaylist), class "btn" ] [ text "Create New Playlist" ] else text "still loading..."
                        , div [ class "debug" ]
                            [ b [] [ text "DEBUG" ]
                            , p [] [ text ("Count of albums: " ++ String.fromInt (List.length global.savedAlbums))]
                            , p [] [ text ("Playlist id: " ++ global.playlistId)]
                            , p [] [ text ("User id: " ++ global.currentUser.id)]
                            , p [] [ text "Error: ", b [] [ text global.errMsg] ]
                            ]
                        ]
                    ]
                ]
            }
        PlaylistInfo _ _ -> { title = "Playlist Stats"
            , body =
                [ div [ class "playlist-stats" ]
                    [ -- Stat.view global stats
                    div [ class "center-panel" ]
                        [
                        b [class "edit-title" ] [ text "User Playlist Stats" ]
                        , div [ class "edit-description" ] 
                            [ p [] [ text "Work in progress to display a users playlists" ]
                            ]
                        --, div [ class "playlist-embed"] [ displayPlaylist global.playlistId ]
                        , div [ class "source-container"] 
                            [ if global.savedPlaylists /= [] then displayAvailablePlaylists global.savedPlaylists else text "still loading..."
                            ]
                        , div [ class "debug" ]
                            [ b [] [ text "DEBUG" ]
                            , p [] [ text ("Count of albums: " ++ String.fromInt (List.length global.savedAlbums))]
                            , p [] [ text ("Playlist id: " ++ global.playlistId)]
                            , p [] [ text ("User id: " ++ global.currentUser.id)]
                            , p [] [ text "Error: ", b [] [ text global.errMsg] ]
                            ]
                        ]
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

displayPlaylist : String ->Html Msg
displayPlaylist playlistId =
    let
        display = playlistId /= ""
    in
        if display then ( Markdown.toHtmlWith { defaultOptions | sanitize = False }
                            [class "playlist-border"]
                            ("""<iframe src="https://open.spotify.com/embed/playlist/""" ++ playlistId ++ """" width="300" height="380" frameborder="0" allowtransparency="true" allow="encrypted-media"></iframe>""") )
        else div [class "playlist-border"] []

displayAvailablePlaylists : List PlaylistSource -> Html Msg
displayAvailablePlaylists availablePlaylists =
    ul [class "source-select"] (List.map (\x -> (checkbox (TogglePlaylistStat x.playlist.id) x.playlist.name)) availablePlaylists)

checkbox : Msg -> String -> Html Msg
checkbox fn name =
    label
        [ style "padding-right" "10px" ]
        [ input [ type_ "checkbox", onClick fn ] []
        , text name
        , br [] []
        ]

viewLink : String -> Html Msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

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
       "-" -> "%2D"
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
                "playlist-stats" -> PlaylistInfo base paramsList
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
