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
import Bitwise exposing (or)


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
              renderStatBlock "Top Genres" "These are the genres most represented in your playlist, along with how many songs belong to that genre (note that spotify bases genre off of artists rather than individual songs)" (topCommonGenres global selectedPlaylists True),
              br [] [],
              text "Select more than one playlist to see comparative stats..."
            ]
          else 
            if notAllPlaylistsLoaded
              then
                text "Loading playlist info..."
              else
                div [ class "playlist-stat-block" ] [ 
                   -- ul [] [text ("Selected playlists: "++String.fromInt(List.length selectedPlaylists))],
                   -- ul [] [text ("Songs Per Playlist:"), div[] (List.map (\x -> div[] [text ( x.playlist.name ++  ": " ++String.fromInt (List.length x.items) ), br [] []]) selectedPlaylists)],
                    -- ul [] (text "Heres some genres" :: List.map (\x -> text ((List.foldl (\a b -> a ++ b) "" (Tuple.second x).genres) ++ (Tuple.first x) ++ " ") ) (Dict.toList global.artistIndex)),
                    renderStatBlock "Top Common Arists" "These are the artists your playlists most often have in common, along with the number of playlists they appear in out of those you selected" (topCommonArtists global selectedPlaylists),
                    renderStatBlock "Top Common Songs" "These are the songs your playlists most often have in common, along with the number of playlists they appear in out of those you selected" (topCommonSongs selectedPlaylists),
                    renderStatBlock "Top Common Genres" "These are the genres that your selected playlists most often have in common, along with the number of playlists they appear in out of those you selected" (topCommonGenres global selectedPlaylists False),
                    renderStatBlock "Top Genres" "These are the genres most often seen on tracks in your playlists, along with how many tracks belong to that genre" (topCommonGenres global selectedPlaylists True),
                    renderStatBlock "Unique Songs" ("These are the songs that appear in the first playlist you selected ("++ (Maybe.withDefault newPlaylistSource (List.head selectedPlaylists)).playlist.name ++") but not in any others") (uniqueSongs selectedPlaylists)
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

renderTopArtist : {count : Int, artistInfo : Artist} -> Int -> String -> Global -> Html msg
renderTopArtist artist rank selectedCount global =
  div [class "top-ten-list-item"] [text ( (String.fromInt (rank + 1)) ++ ". " ++ artist.artistInfo.name ++ " (" ++ String.fromInt artist.count ++ "/"++ selectedCount ++")")
  , br [] []]

renderTopArtistSingle : {count : Int, artistInfo : Artist} -> Int -> String -> Global -> Html msg
renderTopArtistSingle artist rank selectedCount global =
  div [class "top-ten-list-item"] [text ( (String.fromInt (rank + 1)) ++ ". " ++ artist.artistInfo.name ++ " ("++ String.fromInt artist.count ++ ")")
  , br [] []]

topCommonGenres : Global -> List PlaylistSource -> Bool -> Html msg
topCommonGenres global selectedPlaylists notUnique = 
    let
        singlePlaylist = notUnique || (List.length selectedPlaylists == 1)
        artistsOfPlaylists = List.map(\playlist -> List.concatMap (\track -> track.track.artists) playlist.items) selectedPlaylists
        genreAristsOfPlaylists = List.map (\artistList -> List.concatMap (\artist -> List.map (\genre -> {genre= genre, artist= artist}) (Maybe.withDefault newArtist (Dict.get ("spotify:artist:" ++ artist.id) global.artistIndex)).genres) artistList) artistsOfPlaylists
        flatGAP = List.concat genreAristsOfPlaylists
        genresOfPlaylists = List.map (\list -> List.map (\x -> x.genre) list ) genreAristsOfPlaylists
        uniqueGenresOfPlaylists = List.map (\artists -> (uniqueList artists)) genresOfPlaylists
        flatGenreList = List.concat ( if singlePlaylist then genresOfPlaylists else uniqueGenresOfPlaylists )
        emptyCountDict: Dict.Dict String Int
        emptyCountDict = Dict.empty
        countDict = countTopIds (Array.fromList flatGenreList) emptyCountDict
        countList = Dict.toList countDict
        stupidFlippedTuples = List.map (\x -> (Tuple.second x, Tuple.first x)) countList
        sortedCount = Array.fromList ( List.reverse (List.sort stupidFlippedTuples))
        topTen =  Array.slice 0 10 sortedCount
        topTenGenres = Array.map (\x -> {count = Tuple.first x, genre = Tuple.second x}) topTen
        --This is used for tooltip
        topTenGenresWithTooltip = Array.map (\item -> {genre = item.genre, count = item.count, artists = List.map (\x -> x.artist.name) (List.filter (\x -> x.genre == item.genre) flatGAP)}) topTenGenres
        selectedCount = String.fromInt(List.length selectedPlaylists)
        render = if singlePlaylist then renderTopGenreSingle else renderTopGenre
    in
        ul [class "top-common-artists"] (Array.toList (Array.indexedMap (\i x -> (render x i selectedCount global)) topTenGenresWithTooltip))

renderTopGenre : {count : Int, genre : String, artists : List String} -> Int -> String -> Global -> Html msg
renderTopGenre genreInfo rank selectedCount global =
  div [class "top-ten-list-item"] [
      text ( (String.fromInt (rank + 1)) ++ ". " ++ genreInfo.genre ++ " (" ++ String.fromInt genreInfo.count ++ "/"++ selectedCount ++")"),
      span [class "tooltiptext"] [ text ("From artists: " ++ List.foldl (\a b -> a ++", " ++ b) "" genreInfo.artists)],
      br [] []
    ]

renderTopGenreSingle : {count : Int, genre : String, artists : List String} -> Int -> String -> Global -> Html msg
renderTopGenreSingle genreInfo rank selectedCount global =
  div [class "top-ten-list-item"] [
      text ( (String.fromInt (rank + 1)) ++ ". " ++ genreInfo.genre ++ " ("++ String.fromInt genreInfo.count ++ ")"),
      span [class "tooltiptext"] [ text ("From artists: " ++ List.foldl (\a b -> a ++", " ++ b) "" genreInfo.artists)],
      br [] []
    ]

uniqueSongs : List PlaylistSource -> Html msg
uniqueSongs selectedPlaylists = 
    let
        allPlaylistTracks = List.map (\p -> List.map (\x -> x.track) p.items) selectedPlaylists
        --trackDict = listToDict .id allTracksBasic
        allTrackIds = List.map (\p -> List.map (\t -> t.id) p) allPlaylistTracks
        songsFromFirstPlaylist = Maybe.withDefault [] (List.head allPlaylistTracks)
        otherTracks = List.concat ( Maybe.withDefault []  (List.tail allTrackIds))
        unique = List.filter (\s -> not (List.any (\o -> o == s.id) otherTracks)) songsFromFirstPlaylist
    in
        ul [class "top-common-songs"] (List.map renderUniqueSong unique)

renderUniqueSong : Track -> Html msg
renderUniqueSong song =
  div [class "top-ten-list-item"] [text (song.name ++ " by " ++ (listArtists "" song.artists) ++ " off of " ++ song.album.name)
  , br [] []]

renderStatBlock: String -> String -> Html msg -> Html msg
renderStatBlock title description mainElement =
  div [class "playlist-stat"] [
          div [class "stat-title"] [text title],
          div [class "stat-description"] [text description],
          mainElement
  ]
