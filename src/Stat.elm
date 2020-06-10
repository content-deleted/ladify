module Stat exposing (..)
import Stats.GenreGraph exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


{-| Defines a stat to be loaded and displayed 

Stats need things idk I'll define them better later 

-}

type Stat
    = GenreGraph Stats.GenreGraph.Model
    | LeastPopularArtist
    | BPMGraph

view : Global -> List Stat -> Html msg
view global stats = 
    if List.isEmpty global.topTracks.items
        then text "NOT LOADED"
        else ul [] ( List.map renderStat stats )


renderStat : Stat -> Html msg
renderStat stat =
    case stat of
        GenreGraph model -> text "not impemented"
        LeastPopularArtist -> text "not impemented"
        BPMGraph -> text "not impemented"