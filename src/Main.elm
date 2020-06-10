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
import Stats.GenreGraph exposing (Model)

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
        Default  baseUrl params -> ( Model (Global.Global key url "" params "" (TopTrackResponse [])) [], Nav.load
            ("https://accounts.spotify.com/authorize"
            ++ "?client_id=c6494c8623bc4dde928588fc20354bd4" -- consider not doing this
            ++ "&redirect_uri=http%3A%2F%2Flocalhost%3A8000%2Fsrc%2FMain.elm" -- may be smarter to have a specific endpoint 
            ++ "&scope=user-top-read" -- this should change based on what we need, maybe user input?
            ++ "&response_type=token") )

        Authorized baseUrl params -> 
            let
                token = (Maybe.withDefault "" (Dict.get "access_token" params) )
            in
                ( Model (Global.Global key url token params "" (TopTrackResponse [])) [], Http.request { 
                      method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://api.spotify.com/v1/me/top/tracks/?time_range=long_term&limit=50"
                    , body = Http.emptyBody
                    , expect = Http.expectJson GetTracks topTrackResponseDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    } )

-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  let global = model.global in
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.global.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        case urlParser url of
            Default baseUrl params -> ( updateGlobal model { global |  url = url, params = params}, Cmd.none )

            Authorized baseUrl params -> ( updateGlobal model { global |
                    url = url, 
                    params = params, 
                    auth = Maybe.withDefault "" (Dict.get "access_token" params)
                }, Cmd.none )

    GetTracks res -> 
         case res of
                Ok topTracks -> ( updateGlobal model { global | topTracks = topTracks } , Cmd.none)
                Err errorMessage -> ( updateGlobal model { global | errMsg = htmlErrorToString errorMessage } , Cmd.none ) 


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view m =
  let model = m.global in
  { title = "Test"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , p [] [ text "The auth key: ", b [] [ text model.auth] ]
      , if List.isEmpty model.topTracks.items
            then text "NOT LOADED"
            else ul [] (topTracksToImages model.topTracks)
      , p [] [ text "Error: ", b [] [ text model.errMsg] ]
      ]
  }


viewLink : String -> Html Msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

displayImg : String -> Html Msg
displayImg url =  img [ src url, style "width" "200px", style "height" "200px" ] []

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

-- ROUTES 

type Route
  = Default String (Dict String String)
  | Authorized String (Dict String String)

splitPair : String -> (String, String)
splitPair s =
    let
        temp = Array.fromList (String.split "=" s)
        key = Maybe.withDefault "" (Array.get 0 temp)
        value = Maybe.withDefault "" (Array.get 1 temp)
    in 
        (key, value)

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
            Authorized base paramsList
        else
            Default base paramsList
  -- if String.contains "?" (Url.toString url) the


--toRoute : Url.Url -> Route
--toRoute url = 
    --Maybe.withDefault (Default "view") (parse routeParser url)

-- Error handling 
htmlErrorToString : Http.Error -> String
htmlErrorToString error = 
    case error of 
        Http.BadUrl x -> "badurl" ++ x 
        Http.Timeout -> "timout"
        Http.NetworkError -> "networkerror"
        Http.BadStatus x -> "badstatus" ++ String.fromInt x
        Http.BadBody x -> "badbody"++ x
