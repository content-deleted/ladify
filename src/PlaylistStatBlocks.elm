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
import List exposing (sort)


{-| Im not sure what Im doing anymore
-}


view : Global -> Html msg
view global = 
  let
    selectedPlaylists = List.filter (\x -> x.enabled) global.savedPlaylists
    notAllPlaylistsLoaded = List.any (\x -> not x.loaded) selectedPlaylists
    allTracks = List.concatMap (\p -> p.items) selectedPlaylists
  in
    if (List.isEmpty selectedPlaylists)
      then 
        div [ class "playlist-stat-block" ] [ 
              text "Select at least one playlist from your library to see its stats."
            ]
      else
        if (List.length selectedPlaylists) < 2
          then 
            div [ class "playlist-stat-block" ] [ 
              renderStatBlock "Top Artists" "These are the artists that appear most frequently in the playlist you selected, and how many songs by those artists are in your playlist" (topCommonArtists global selectedPlaylists),
              br [] [],
              text "Select more than one playlist to see comparative stats..."
            ]
          else 
            if notAllPlaylistsLoaded
              then
                text "Loading playlist info..."
              else
                div [ class "playlist-stat-block" ] [ 
                    ul [] [text ("Selected playlists: "++String.fromInt(List.length selectedPlaylists))],
                    ul [] [text ("Songs Per Playlist:"), div[] (List.map (\x -> div[] [text ( x.playlist.name ++  ": " ++String.fromInt (List.length x.items) ), br [] []]) selectedPlaylists)],
                    -- ul [] (text "Heres some genres" :: List.map (\x -> text ((List.foldl (\a b -> a ++ b) "" (Tuple.second x).genres) ++ (Tuple.first x) ++ " ") ) (Dict.toList global.artistIndex)),
                    renderStatBlock "Top Common Arists" "These are the artists your playlists most often have in common, along with the number of playlists they appear in out of those you selected" (topCommonArtists global selectedPlaylists),
                    renderStatBlock "Top Common Songs" "These are the songs your playlists most often have in common, along with the number of playlists they appear in out of those you selected" (topCommonSongs selectedPlaylists)
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
        countDict = countTopIds allTrackIds emptyCountDict
        countList = Dict.toList countDict
        stupidFlippedTuples = List.map (\x -> (Tuple.second x, Tuple.first x)) countList
        sortedCount = Array.fromList ( List.reverse (List.sort stupidFlippedTuples))
        topTen =  Array.slice 0 10 sortedCount
        topTenTracks = Array.map (\x -> {count = Tuple.first x, trackInfo = Maybe.withDefault newTrack (Dict.get (Tuple.second x) trackDict)}) topTen
        selectedCount = String.fromInt(List.length selectedPlaylists)
    in
        ul [class "top-common-songs"] (Array.toList (Array.indexedMap (\i x -> (renderTopSong x i selectedCount)) topTenTracks))

renderTopSong : {count : Int, trackInfo : Track} -> Int -> String -> Html msg
renderTopSong song rank selectedCount =
  div [class "top-ten-list-item"] [text ((String.fromInt (rank + 1)) ++ ". " ++song.trackInfo.name ++ " by " ++ (listArtists "" song.trackInfo.artists) ++ " off of " ++ song.trackInfo.album.name ++ " (" ++ String.fromInt song.count ++ "/"++ selectedCount ++")")
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

topCommonArtists : Global -> List PlaylistSource -> Html msg
topCommonArtists global selectedPlaylists = 
    let
        singlePlaylist = List.length selectedPlaylists == 1
        artistsOfPlaylists = List.map(\playlist -> List.concatMap (\track -> track.track.artists) playlist.items) selectedPlaylists
        uniqueArtistsOfPlaylists = List.map (\artists -> (uniqueList artists)) artistsOfPlaylists
        flatArtistList = List.concat ( if singlePlaylist then artistsOfPlaylists else uniqueArtistsOfPlaylists )
        allArtistIds = Array.fromList ( List.map (\t -> t.id) flatArtistList)
        emptyCountDict: Dict.Dict String Int
        emptyCountDict = Dict.empty
        countDict = countTopIds allArtistIds emptyCountDict
        countList = Dict.toList countDict
        stupidFlippedTuples = List.map (\x -> (Tuple.second x, Tuple.first x)) countList
        sortedCount = Array.fromList ( List.reverse (List.sort stupidFlippedTuples))
        topTen =  Array.slice 0 10 sortedCount
        topTenArtists = Array.map (\x -> {count = Tuple.first x, artistInfo = Maybe.withDefault newArtist (Dict.get ("spotify:artist:" ++ (Tuple.second x)) global.artistIndex)}) topTen
        selectedCount = String.fromInt(List.length selectedPlaylists)
        render = if singlePlaylist then renderTopArtistSingle else renderTopArtist
    in
        -- text (String.fromInt (Dict.size global.artistIndex))
        --ul [] [
        --  ul [] (List.map (\x -> text (Tuple.first x)) (Dict.toList global.artistIndex)),
        --  ul [] (Array.toList (Array.map (\x -> text (" " ++ Tuple.second x)) topTen))
        --]
        ul [class "top-common-artists"] (Array.toList (Array.indexedMap (\i x -> (render x i selectedCount global)) topTenArtists))

countTopIds : Array.Array String -> Dict.Dict String Int -> Dict.Dict String Int
countTopIds remainingIds idCounts =
  if Array.length remainingIds == 0
    then
      idCounts
    else
      let
        curId = Maybe.withDefault "" (Array.get ((Array.length remainingIds) - 1) remainingIds)
        prevCount = Maybe.withDefault 0 (Dict.get curId idCounts)
        newIdCounts = Dict.insert curId (prevCount+1) idCounts
      in
        countTopIds (Array.slice 0 -1 remainingIds) newIdCounts

renderTopArtist : {count : Int, artistInfo : Artist} -> Int -> String -> Global -> Html msg
renderTopArtist artist rank selectedCount global =
  div [class "top-ten-list-item"] [text ( (String.fromInt (rank + 1)) ++ ". " ++ artist.artistInfo.name ++ " (" ++ String.fromInt artist.count ++ "/"++ selectedCount ++")")
  , br [] []]

renderTopArtistSingle : {count : Int, artistInfo : Artist} -> Int -> String -> Global -> Html msg
renderTopArtistSingle artist rank selectedCount global =
  div [class "top-ten-list-item"] [text ( (String.fromInt (rank + 1)) ++ ". " ++ artist.artistInfo.name ++ " ("++ String.fromInt artist.count ++ ")")
  , br [] []]

renderStatBlock: String -> String -> Html msg -> Html msg
renderStatBlock title description mainElement =
  div [class "playlist-stat"] [
          div [class "stat-title"] [text title],
          div [class "stat-description"] [text description],
          mainElement
  ]
