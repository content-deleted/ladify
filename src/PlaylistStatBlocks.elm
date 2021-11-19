module PlaylistStatBlocks exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Array
import Array exposing (empty)
import Dict exposing (Dict)
import Array exposing (Array)
import Json.Decode exposing (array)


{-| Im not sure what Im doing anymore
-}


view : Global -> Html msg
view global = 
  let
    selectedPlaylists = List.filter (\x -> x.enabled) global.savedPlaylists
    notAllPlaylistsLoaded = List.any (\x -> not x.loaded) selectedPlaylists
    allTracks = List.concatMap (\p -> p.items) selectedPlaylists
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
                ul [] [text ("Selected playlists: "++String.fromInt(List.length selectedPlaylists))],
                ul [] [text ("Songs Per Playlist:"), div[] (List.map (\x -> div[] [text ( x.playlist.name ++  ": " ++String.fromInt (List.length x.items) ), br [] []]) selectedPlaylists)],
                -- ul [] (text "Heres some genres" :: List.map (\x -> text ((List.foldl (\a b -> a ++ b) "" (Tuple.second x).genres) ++ (Tuple.first x) ++ " ") ) (Dict.toList global.artistIndex)),
                topCommonSongs selectedPlaylists
                -- THIS PRINTS ALL SONGS: ul [] (List.map (\x -> div[] [text ("Id: " ++ x.track.id ++ " name: " ++ x.track.name), br [] []]) allTracks)
            ]

listToDict : (a -> comparable) -> List a -> Dict.Dict comparable a
listToDict getKey values = Dict.fromList (List.map (\v -> (getKey v, v)) values)

topCommonSongs : List PlaylistSource -> Html msg
topCommonSongs selectedPlaylists = 
    let
        allTracks = List.concatMap (\p -> p.items) selectedPlaylists
        allTracksBasic = List.map (\p -> p.track) allTracks
        trackDict = listToDict .id allTracksBasic
        allTrackIds = Array.fromList ( List.map (\t -> t.id) allTracksBasic)
        emptyCountDict: Dict.Dict String Int
        emptyCountDict = Dict.empty
        countDict = countTopSongs allTrackIds emptyCountDict
        countList = Dict.toList countDict
        stupidFlippedTuples = List.map (\x -> (Tuple.second x, Tuple.first x)) countList
        sortedCount = Array.fromList ( List.reverse (List.sort stupidFlippedTuples))
        topTen =  Array.slice 0 10 sortedCount
        topTenTracks = Array.map (\x -> {count = Tuple.first x, trackInfo = Maybe.withDefault newTrack (Dict.get (Tuple.second x) trackDict)}) topTen
        selectedCount = String.fromInt(List.length selectedPlaylists)
    in
        ul [class "top-common-songs"] (Array.toList (Array.map (\x -> (renderTopSong x selectedCount)) topTenTracks))

countTopSongs : Array.Array String -> Dict.Dict String Int -> Dict.Dict String Int
countTopSongs remainingTracks songCounts =
  if Array.length remainingTracks == 0
    then
      songCounts
    else
      let
        curId = Maybe.withDefault "" (Array.get ((Array.length remainingTracks) - 1) remainingTracks)
        prevCount = Maybe.withDefault 0 (Dict.get curId songCounts)
        newSongCounts = Dict.insert curId (prevCount+1) songCounts
      in
        countTopSongs (Array.slice 0 -1 remainingTracks) newSongCounts

renderTopSong : {count : Int, trackInfo : Track} -> String -> Html msg
renderTopSong song selectedCount =
  div [] [text (song.trackInfo.name ++ " by " ++ (listArtists "" song.trackInfo.artists) ++ " off of " ++ song.trackInfo.album.name ++ " (" ++ String.fromInt song.count ++ "/"++ selectedCount ++")")
  , br [] []]

listArtists : String -> List SimpleArtist -> String
listArtists previous artists =
  let
    base = if previous == "" then "" else previous ++ " and "
    artist = Maybe.withDefault (SimpleArtist "" "" "") (List.head artists)
    remaining = Maybe.withDefault [] (List.tail artists)
  in
    if List.length artists == 0
      then
        previous
      else
        listArtists (base ++ artist.name) remaining