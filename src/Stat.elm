module Stat exposing (..)
import Stats.GenreGraph exposing (..)
import Stats.Decades exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


{-| Defines type and view information used by all stats

A Stat represent some block of information about your spotify data
We want to eventually have pagination for this so that we're not bothering to update and render all of them at once
 -> possibly 2 to a page?

Stats need at least a view function but they may have models as well

-}

type Stat
    = GenreGraph Stats.GenreGraph.Model
    | TrackPopularity
    | Decades

init : List Stat
init = [ Decades, GenreGraph { x = 0, y = 0 } ]

view : Global -> List Stat -> Html msg
view global stats = 
    if List.isEmpty global.topTracks.items
        then text "NOT LOADED"
        else div [ class "stat-list" ] [ 
            ul [] ( List.map (renderStat global) stats )
        ]


renderStat : Global -> Stat -> Html msg
renderStat global stat =
    div [ class "stat-wrapper" ] [ 
        case stat of
            GenreGraph model -> Stats.GenreGraph.view model
            TrackPopularity -> text "not impemented"
            Decades ->  Stats.Decades.view global
    ]