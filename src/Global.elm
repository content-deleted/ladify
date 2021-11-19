module Global exposing (..)
import Browser.Navigation as Nav
import Url
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Http exposing (Error)
import Browser
import Array


-- MODEL
type alias Global =
  { key : Nav.Key
  , url : Url.Url
  , auth : String
  , params : (Dict String String)
  , errMsg : String
  , topTracks : TopTrackResponse
  , currentRoute : Route
  , savedAlbums : List Album
  , playlistId : String
  , currentUser : User
  , dataSources : DataSources
  , savedPlaylists : List PlaylistSource
  , albumIndex : Dict.Dict String Album
  , artistIndex : Dict.Dict String Artist
  }

-- This will be used for a checkbox interface to let users select which sources should be used for generating a Library playlist
-- userPlaylist source should ignore playlist of our naming convention probably, but we dont add duplicates so its not really a problem either way, just stupid
type alias DataSources =
 { savedAlbums : Bool
 , savedArtists : Bool
 , userPlaylists : Bool
 , likedSongs : Bool
 }

newDataSources : DataSources
newDataSources = DataSources False False False False

-- ROUTES 
type Route
  = Unauthorized String (Dict String String)
  | StatDisplay String (Dict String String)
  | PlaylistEdit String (Dict String String)
  | PlaylistInfo String (Dict String String)

-- MESSAGE
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GetTracks (Result Http.Error TopTrackResponse)
  | GetLibraryAlbums (Result Http.Error TopAlbumsResponse)
  | CreatePlaylist (Result Http.Error Playlist)
  | SendSpotifyRequest SpotifyRequest
  | ProcessAddSongsToPlaylist (Msg) (Result Http.Error ())
  | GetUser (Result Http.Error User)
  | TogglePlaylistStat String
  | GetUserPlaylists (Result Http.Error UserPlaylistsResponse)
  | GetPlaylistTracks PlaylistSource (Result Http.Error PlaylistItemsResponse)
  | WriteAlbumIndex (Result Http.Error Album)
  | WriteArtistIndex (Result Http.Error Artist)

type SpotifyRequest
  = RequestCreatePlaylist
  | RequestAddSongsPlaylist (Array.Array String)

-- Decode Song Response 
type alias TopAlbumsResponse =  { items: List SavedAlbum, next: String}
type alias TopTrackResponse =  { items: List Track}
 
newTrack : Track
newTrack = Track "" "" newSimpleAlbum [] 0 "" False

type alias Track =
  { name : String
  , id : String
  , album : SimpleAlbum
  , artists : List SimpleArtist
  , popularity : Int
  , preview_url : String
  , explicit : Bool
  }

type alias SimpleTrack =
  { name : String
  , id : String
  }

newSimpleAlbum : SimpleAlbum
newSimpleAlbum = SimpleAlbum "" "" "" []
type alias SimpleAlbum =
  { id : String
  , name : String
  , release_date : String
  , images : List AlbumArt
  }

type alias AlbumTracksWrapper = { items : List SimpleTrack }

type alias SavedAlbum = 
    { album : Album
    , added_at : String
    }
type alias Album =
  { name : String
  , id : String
  , genres : List String
  , release_date : String
  , images : List AlbumArt
  , tracks : AlbumTracksWrapper
  }
type alias AlbumArt =
  { url : String
  , height : Int
  , width : Int
  }

type alias SimpleArtist =
  { name : String
  , uri : String
  , id : String
  }

newArtist : Artist
newArtist = Artist "" "" "" [] [] {total=0}
type alias Artist =
  { name : String
  , id : String
  , uri : String
  , genres : List String
  , images : List AlbumArt
  , followers : ArtistFollowers
  }

type alias ArtistFollowers = { total : Int }

type alias AddedBy =
  { id : String
  , href : String
  }
type alias UserPlaylistsResponse =  { items: List Playlist, next: String}

type alias PlaylistItemsResponse = { items: List PlaylistTrack, next: String}

-- This type is used for toggling what 
type alias PlaylistSource = { playlist: Playlist, enabled: Bool, loaded: Bool, items: List PlaylistTrack }
type alias PlaylistTrack =  { track: Track, addedBy: AddedBy }

userPlaylistsResponseDecoder : Decoder UserPlaylistsResponse
userPlaylistsResponseDecoder =
  Json.Decode.map2 UserPlaylistsResponse
    (field "items" (Json.Decode.list playlistDecoder))
    (field "next" (oneOf [ string, null "none" ]) )

playlistItemsResponseDecoder : Decoder PlaylistItemsResponse
playlistItemsResponseDecoder =
  Json.Decode.map2 PlaylistItemsResponse
    (field "items" (Json.Decode.list playlistTrackDecoder))
    (field "next" (oneOf [ string, null "none" ]) )

playlistTrackDecoder : Decoder PlaylistTrack
playlistTrackDecoder =
  Json.Decode.map2 PlaylistTrack
    (field "track" tracksDecoder)
    (field "added_by" addedByDecoder)

addedByDecoder : Decoder AddedBy
addedByDecoder =
  Json.Decode.map2 AddedBy
    (field "id" string)
    (field "href" string)

topAlbumsResponseDecoder : Decoder TopAlbumsResponse
topAlbumsResponseDecoder =
  Json.Decode.map2 TopAlbumsResponse
      (field "items" (Json.Decode.list savedAlbumDecoder))
      (field "next" (oneOf [ string, null "none" ]) )

savedAlbumDecoder : Decoder SavedAlbum
savedAlbumDecoder =
  Json.Decode.map2 SavedAlbum
      (field "album" albumsDecoder)
      (field "added_at" string)

topTrackResponseDecoder : Decoder TopTrackResponse
topTrackResponseDecoder =
  Json.Decode.map TopTrackResponse
      (field "items" (Json.Decode.list tracksDecoder))

tracksDecoder : Decoder Track
tracksDecoder =
  Json.Decode.map7 Track
    (field "name" string)
    (field "id" string)
    (field "album" simpleAlbumsDecoder)
    (field "artists" (Json.Decode.list simpleArtistDecoder))
    (field "popularity" int)
    (field "preview_url" (oneOf [ string, null "EMPTY" ]) )
    (field "explicit" bool)


simpleTracksDecoder : Decoder SimpleTrack
simpleTracksDecoder =
  Json.Decode.map2 SimpleTrack
    (field "name" string)
    (field "id" string)

simpleAlbumsDecoder : Decoder SimpleAlbum
simpleAlbumsDecoder =
  Json.Decode.map4 SimpleAlbum
    (field "id" string)
    (field "name" string)
    (field "release_date" string)
    (field "images" (Json.Decode.list albumArtDecoder))

albumTracksWrapperDecoder : Decoder AlbumTracksWrapper
albumTracksWrapperDecoder =
  Json.Decode.map AlbumTracksWrapper
    (field "items" (Json.Decode.list simpleTracksDecoder))

albumsDecoder : Decoder Album
albumsDecoder =
  Json.Decode.map6 Album
    (field "name" string)
    (field "id" string)
    (field "genres" (Json.Decode.list string))
    (field "release_date" string)
    (field "images" (Json.Decode.list albumArtDecoder))
    (field "tracks" albumTracksWrapperDecoder)

artistDecoder : Decoder Artist
artistDecoder =
  Json.Decode.map6 Artist
    (field "name" string)
    (field "uri" string)
    (field "id" string)
    (field "genres" (Json.Decode.list string))
    (field "images" (Json.Decode.list albumArtDecoder))
    (field "followers" artistFollowersDecoder)

artistFollowersDecoder : Decoder ArtistFollowers
artistFollowersDecoder = 
  Json.Decode.map ArtistFollowers
    (field "total" int)

albumArtDecoder : Decoder AlbumArt
albumArtDecoder = 
    Json.Decode.map3 AlbumArt
      (field "url" string)
      (field "width" int)
      (field "height" int)


simpleArtistDecoder : Decoder SimpleArtist
simpleArtistDecoder =
  Json.Decode.map3 SimpleArtist
    (field "name" string)
    (field "uri" string)
    (field "id" string)


-- Playlists

type alias Playlist =
  { name : String
  , id : String
  }

playlistDecoder : Decoder Playlist
playlistDecoder =
  Json.Decode.map2 Playlist
      (field "name" string)
      (field "id" string)


-- Users

type alias User =
  { display_name : String
  , id : String
  }

-- Empty Initilizer 
newUser : User
newUser = User "" ""

userDecoder : Decoder User
userDecoder =
  Json.Decode.map2 User
      (field "display_name" string)
      (field "id" string)

-- Helpers 

uniqueList : List a -> List a
uniqueList l = 
    let
        incUnique : a -> List a -> List a
        incUnique elem lst = 
            case List.member elem lst of
                True -> lst
                False -> elem :: lst
    in
        List.foldr incUnique [] l