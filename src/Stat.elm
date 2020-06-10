module Stat exposing (..)
import Stats.GenreGraph exposing (..)

{-| Defines a stat to be loaded and displayed 

Stats need things idk I'll define them better later 

-}

type Stat
    = GenreGraph
    | LeastPopularArtist
    | BPMGraph

type alias Model = 
    { genre : Stats.GenreGraph.Model
    }