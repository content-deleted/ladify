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
  , artists : List SimpleArtist
  , popularity : Int
  , preview_url : String
  , explicit : Bool
  }
type alias Album =
  { name : String
  , release_date : String
  , images : List AlbumArt
  }
type alias AlbumArt =
  { url : String
  , height : Int
  , width : Int
  }

type alias SimpleArtist =
  { name : String
  , uri : String
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
  Json.Decode.map6 Track
    (field "name" string)
    (field "album" albumsDecoder)
    (field "artists" (Json.Decode.list artistDecoder))
    (field "popularity" int)
    (field "preview_url" (oneOf [ string, null "EMPTY" ]) )
    (field "explicit" bool)

albumsDecoder : Decoder Album
albumsDecoder =
  Json.Decode.map3 Album
    (field "name" string)
    (field "release_date" string)
    (field "images" (Json.Decode.list albumArtDecoder))

albumArtDecoder : Decoder AlbumArt
albumArtDecoder = 
    Json.Decode.map3 AlbumArt
      (field "url" string)
      (field "width" int)
      (field "height" int)


artistDecoder : Decoder SimpleArtist
artistDecoder =
  Json.Decode.map2 SimpleArtist
    (field "name" string)
    (field "uri" string)