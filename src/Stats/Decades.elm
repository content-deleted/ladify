module Stats.Decades exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

view : Global -> Html msg
view global = 
  ul [] (List.map printNameAndYear global.topTracks.items)

printNameAndYear : Track -> Html msg
printNameAndYear track =
  text (track.album.name ++ "-" ++ track.album.release_date ++ " ")