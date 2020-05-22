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



-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , auth : String
  , params : (Dict String String)
  , topAlbums : LoadedAlbum
  , errMsg : String
  }

type LoadedAlbum
    = NotLoaded 
    | Loaded TopAlbumsResponse

-- INIT 

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case urlParser url of
        Default baseUrl params -> ( Model key url "" params NotLoaded "", Nav.load
            ("https://accounts.spotify.com/authorize"
            ++ "?client_id=c6494c8623bc4dde928588fc20354bd4" -- consider not doing this
            ++ "&redirect_uri=http%3A%2F%2Flocalhost%3A8000%2Fsrc%2FMain.elm" -- may be smarter to have a specific endpoint 
            ++ "&scope=user-top-read" -- this should change based on what we need, maybe user input?
            ++ "&response_type=token") )

        Authorized baseUrl params -> 
            let
                token = (Maybe.withDefault "" (Dict.get "access_token" params) )
            in
                ( Model key url token params NotLoaded "", Http.request { 
                      method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
                    , url = "https://api.spotify.com/v1/me/top/albums"
                    , body = Http.emptyBody
                    , expect = Http.expectJson GetAlbums topAlbumsResponseDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    } )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GetAlbums (Result Http.Error TopAlbumsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        case urlParser url of
            Default baseUrl params -> ( { model | url = url, params = params}, Cmd.none )

            Authorized baseUrl params -> ( { model |
                    url = url, 
                    params = params, 
                    auth = Maybe.withDefault "" (Dict.get "access_token" params)
                }, Cmd.none )

    GetAlbums res -> 
         case res of
                Ok topAlbums -> ( { model | topAlbums = Loaded topAlbums } , Cmd.none)
                Err errorMessage -> 
                 case errorMessage of 
                    Http.BadUrl x -> ( { model | errMsg = "badurl" ++ x } , Cmd.none)
                    Http.Timeout -> ( { model | errMsg = "timout" } , Cmd.none)
                    Http.NetworkError -> ( { model | errMsg = "networkerror" } , Cmd.none)
                    Http.BadStatus x -> ( { model | errMsg = "badstatus" ++ String.fromInt x} , Cmd.none)
                    Http.BadBody x -> ( { model | errMsg = "badbody"++x } , Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Test"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , p [] [ text "The auth key: ", b [] [ text model.auth] ]
      , case model.topAlbums of
            Loaded albums -> ul [] (topAlbumsToImages albums)
            NotLoaded -> text "NOT LOADED"
      , p [] [ text "Error: ", b [] [ text model.errMsg] ]
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

displayImg : String -> Html msg
displayImg url = img [ src url ] []

topAlbumsToImages : TopAlbumsResponse -> List (Html msg)
topAlbumsToImages res =
    let
        firstArts = List.map (\x -> Maybe.withDefault (AlbumArt "") (List.head x.images) ) res.items 
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


-- Decode Song Response 
type alias TopAlbumsResponse =  { items: List Album}
type alias Album =
  { name : String
  , images : List AlbumArt
  }
type alias AlbumArt =
  { --height : Int
 -- , width : Int
  url : String
  }

topAlbumsResponseDecoder : Decoder TopAlbumsResponse
topAlbumsResponseDecoder =
  Json.Decode.map TopAlbumsResponse
      (field "items" (Json.Decode.list albumsDecoder))

albumsDecoder : Decoder Album
albumsDecoder =
  Json.Decode.map2 Album
    (field "name" string)
    (field "images" (Json.Decode.list albumArtDecoder))

albumArtDecoder : Decoder AlbumArt
albumArtDecoder = 
    Json.Decode.map AlbumArt
      (field "url" string)