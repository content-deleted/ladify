module PlaylistStatBlocks exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


{-| Im not sure what Im doing anymore
-}


view : Global -> Html msg
view global = 
  let
    selectedPlaylists = List.filter (\x -> x.enabled) global.savedPlaylists
    notAllPlaylistsLoaded = List.any (\x -> x.loaded) selectedPlaylists
  in
    if (List.length selectedPlaylists) < 2
      then 
        text "First select playlists"
      else 
        if notAllPlaylistsLoaded
          then
            text "Loading playlist info..."
          else
            div [ class "playlist-stat-block" ] [ 
                ul [] [text (""++String.fromInt(List.length selectedPlaylists))]
            ]


topCommonSongs : List PlaylistSource -> Html msg
topCommonSongs selectedPlaylists = 
    let
        x = False
    in
        div [ class "test" ] []