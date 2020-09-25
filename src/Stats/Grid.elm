module Stats.Grid exposing (..)

import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode exposing (string)
import Result exposing (Result)
import Maybe exposing (Maybe)
import Array exposing (indexedMap)

view : Global -> Html msg
view global = 
  let 
    tracks = global.topTracks.items
    years = List.map getYear tracks
  in 
    div [ class "grid-wrapper" ] [
      ul [] (List.map printNameAndYear global.topTracks.items)
      , ul [] (List.map (\y -> text (y ++ " ") )  years) 
      --, ul [] (List.map (\y -> text (String.fromInt (getBucket y)  ++ " ") )  years)  
      , ul [] (List.map printNameAndYear global.topTracks.items)
      , ul [] (topTracksToImages tracks)
    ]
printNameAndYear : Track -> Html msg
printNameAndYear track =
  text (track.album.name ++ "-" ++ track.album.release_date ++ " ")

getYear : Track -> String 
getYear track = String.slice 0 4 track.album.release_date

topTracksToImages : List (Track) -> List (Html msg)
topTracksToImages tracks =
    let
        firstArts = List.map extractAlbumArt tracks 
    in
        List.map (\a -> displayImg a.url) firstArts


displayImg : String -> Html msg
displayImg url =  img [ src url, style "width" "200px", style "height" "200px" ] []

extractAlbumArt : Track -> AlbumArt
extractAlbumArt track = Maybe.withDefault (AlbumArt "" 1 1) (List.head track.album.images)
