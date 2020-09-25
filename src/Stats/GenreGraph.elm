module Stats.GenreGraph exposing (..)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, defaultOptions, Error)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL 
-- Lets update this to be a little cleaner 
type alias Model = 
    { x : Int
    , y : Int
    } 

-- View
view : Model -> Html msg
view { x, y } = 
  div [class "genre-wrapper"] [
    WebGL.toHtml
      [ width 500
      , height 500
      , style "display" "block"
      , style "position" "absolute"
      , style "left" "0"
      , style "top" "0"
      ]
      [ toEntity faceMesh
      , toEntity sidesMesh
      ]
  ]


-- Mesh
type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


faceMesh : Mesh Vertex
faceMesh =
    WebGL.triangles square


sidesMesh : Mesh Vertex
sidesMesh =
    [ ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, 270 ) ]
        |> List.concatMap rotatedSquare
        |> WebGL.triangles


rotatedSquare : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedSquare ( angleXZ, angleYZ ) =
    let
        transformMat =
            Mat4.mul
                (Mat4.makeRotate (degrees angleXZ) Vec3.j)
                (Mat4.makeRotate (degrees angleYZ) Vec3.i)

        transform vertex =
            { vertex
                | position =
                    Mat4.transform transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
        List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
    let
        topLeft =
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]

toEntity : Mesh Vertex -> Entity
toEntity mesh =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = perspective 500 500 0.0 0.0
        }


perspective : Float -> Float -> Float -> Float -> Mat4
perspective width height x y =
    let
        eye =
            vec3 (0.5 - x / width) -(0.5 - y / height) 1
                |> Vec3.normalize
                |> Vec3.scale 6
    in
        Mat4.mul
            (Mat4.makePerspective 45 (width / height) 0.01 100)
            (Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j)

-- SHADERS


type alias Uniforms =
    { perspective : Mat4
    }


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        varying vec2 vcoord;
        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcoord = coord.xy;
        }
    |]


fragmentShader : Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 vcoord;
        void main () {
          gl_FragColor = vec4(0.3, 0.2, 0.7, 1.0);
        }
    |]