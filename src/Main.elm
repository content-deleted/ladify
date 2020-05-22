module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Array
import Dict exposing (Dict)

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
  }


-- INIT 

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case urlParser url of
        Default baseUrl params -> ( Model key url "" params, Cmd.none )

        Authorized baseUrl params -> ( Model key url (Maybe.withDefault "" (Dict.get "auth" params) ) params, Cmd.none )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


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
                    auth = Maybe.withDefault "" (Dict.get "auth" params)
                }, Cmd.none )



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
      , ul []
          [ viewLink "/test"
          ]
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]



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
containsAuth p = Dict.member "auth" p

urlParser : Url.Url -> Route
urlParser url =
    let
        temp = Array.fromList (String.split "?" (Url.toString url))
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