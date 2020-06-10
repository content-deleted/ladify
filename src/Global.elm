module Global exposing (..)
import Browser.Navigation as Nav
import Url
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Http exposing (Error)
import Browser

-- MODEL
type alias Global =
  { key : Nav.Key
  , url : Url.Url
  , auth : String
  , params : (Dict String String)
  , errMsg : String
  , topTracks : TopTrackResponse
  }


-- MESSAGE
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GetTracks (Result Http.Error TopTrackResponse)

-- Decode Song Response 
type alias TopAlbumsResponse =  { items: List Album}
type alias TopTrackResponse =  { items: List Track}

type alias Track =
  { name : String
  , album : Album
  }
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

topTrackResponseDecoder : Decoder TopTrackResponse
topTrackResponseDecoder =
  Json.Decode.map TopTrackResponse
      (field "items" (Json.Decode.list tracksDecoder))

tracksDecoder : Decoder Track
tracksDecoder =
  Json.Decode.map2 Track
    (field "name" string)
    (field "album" albumsDecoder)

albumsDecoder : Decoder Album
albumsDecoder =
  Json.Decode.map2 Album
    (field "name" string)
    (field "images" (Json.Decode.list albumArtDecoder))

albumArtDecoder : Decoder AlbumArt
albumArtDecoder = 
    Json.Decode.map AlbumArt
      (field "url" string)