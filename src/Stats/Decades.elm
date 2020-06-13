module Stats.Decades exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Array
import Json.Encode exposing (string)
import Result exposing (Result)
import Maybe exposing (Maybe)
import Array exposing (indexedMap)
import Json.Encode exposing (int)

view : Global -> Html msg
view global = 
  let years = List.map getYear global.topTracks.items
  in 
    div [] [
      dl [ class "graph" ] 
      (List.concat [ [ dt [ class "title" ] [text "Top Tracks by Decade"] ] 
        , generateBars global.topTracks.items ] )
      -- , ul [] (List.map printNameAndYear global.topTracks.items)
      -- , ul [] (List.map (\y -> text (y ++ " ") )  years) 
      -- , ul [] (List.map (\y -> text (String.fromInt (getBucket y)  ++ " ") )  years)
    ]
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
      years = List.map getYear tracks
      result = years |> List.foldl dropIn buckets --(Array.fromList (List.map (\x -> String.toInt x |> Maybe.withDefault 2020) years))
  in 
    Array.toList (indexedMap (\i count -> drawBar (1930 + i * 10) count ) result )

dropIn : String -> Array.Array Int -> Array.Array Int
dropIn = (\year current -> Array.set (getBucket year) ((Array.get (getBucket year) current |> Maybe.withDefault 0) + 10) current ) 

drawBar : Int -> Int -> Html msg
drawBar decade count = dd [ class "bar", style "height" (String.fromInt count ++ "px") ]
              [ span [ class "label" ] [text (String.fromInt decade)]
            ] 

getBucket : String -> Int
getBucket yearStr = 
  let 
    year = String.toInt yearStr |> Maybe.withDefault 2020
  in 
    if year < 1930 then 
      0
    else 
      (year - 1930) // 10

getYear : Track -> String 
getYear track = String.slice 0 4 track.album.release_date