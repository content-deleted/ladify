module Stats.Decades exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Array
import Json.Encode exposing (string)
import Result exposing (Result)
import Maybe exposing (Maybe)
import Array exposing (indexedMap)

view : Global -> Html msg
view global = 
  dl [ class "graph" ] 
    (List.concat [ [ dt [ class "title" ] [text "Title"] ] 
      , generateBars global.topTracks.items ] )
  --ul [] (List.map printNameAndYear global.topTracks.items)

printNameAndYear : Track -> Html msg
printNameAndYear track =
  text (track.album.name ++ "-" ++ track.album.release_date ++ " ")

-- generate one bar for each of the last 10 decades
-- place the tracks into decade buckets and count
generateBars : (List Track) -> List (Html msg)
generateBars tracks = 
  let
      buckets = Array.repeat 10 0 -- (List.map (\x -> (x * 10) - 2020) (List.range 0 10))
      years = List.map (\track -> (String.slice 0 3 track.album.release_date) ) tracks
      x = List.map (\year -> Array.set (getBucket year) (Array.get ((getBucket year) + 1) buckets |> Maybe.withDefault 0) buckets ) years
  in 
    Array.toList (indexedMap (\i count -> drawBar (1930 + i * 10) count ) buckets )


drawBar : Int -> Int -> Html msg
drawBar decade count = dd [ class "bar", style "height" (String.fromInt count ++ "px") ]
              [ span [ class "label" ] [text (String.fromInt decade)]
            ] 

getBucket : String -> Int
getBucket yearStr = 
  let year = String.toInt yearStr |> Maybe.withDefault 2020
  in 
    if year < 1930 then 
      0
    else 
      (year - 1930) // 10