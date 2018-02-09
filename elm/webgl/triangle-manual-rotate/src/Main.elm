import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import AnimationFrame

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)

{-
[An Introduction to Elm-WebGL](http://unsoundscapes.com/slides/2016-06-06-introduction-to-elm-webgl)
のデモを写経。ただし、elm-community/elm-webgl から elm-community/webgl にあわせて変えてある。

三角形がぐるぐる回る。
-}

type alias Model = Float

--main : Program Never Time Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = \model -> Sub.none --(\model -> AnimationFrame.diffs Basics.identity)
        , update = update
        }

type Msg
    = RotatePlus
    | RotateMinus

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RotatePlus  -> (model + 10, Cmd.none)
        RotateMinus -> (model - 10, Cmd.none)


------------------------------------------------------------
-- Render
------------------------------------------------------------

view : Model -> Html Msg
view angle =
    div [] [ h1 [] [text "くるくる回るよ"]
           , WebGL.toHtml
               [ width 300
               , height 300
               , style [("display", "block")]
               ]
                 [ WebGL.entity
                       vertexShader
                       fragmentShader
                       triangle
                       { rotation = makeRotate (degrees angle) (vec3 0 1 0) }
                 ]
           , p [] ["deg:" ++ (angle |> toString)  |>  text]
           , div [] [ button [onClick RotatePlus] [text "+"]
                    , button [onClick RotateMinus] [text "-"]
                    ]
           ]


------------------------------------------------------------
-- Mesh
------------------------------------------------------------

type alias Vertex =
    { position : Vec3
    , color : Vec3
    }

triangle : Mesh Vertex
triangle =
    WebGL.triangles
        [ ( Vertex (vec3 0 1 0) (vec3 1 0 0)
          , Vertex (vec3 -1 -1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]

------------------------------------------------------------
-- Shaders
------------------------------------------------------------

-- Vertex Shader
type alias Uniform = { rotation : Mat4 }
type alias Varying = { vColor : Vec3 }

vertexShader : Shader Vertex Uniform Varying
vertexShader = [glsl|
  attribute vec3 position;
  attribute vec3 color;
  varying vec3 vColor;
  uniform mat4 rotation;
  void main () {
    gl_Position = rotation * vec4(position, 1.0);
    vColor = color;
  }
|]

-- Fragment Shader

fragmentShader : Shader {} Uniform Varying
fragmentShader = [glsl|
  precision mediump float;
  varying vec3 vColor;
  void main () {
    gl_FragColor = vec4(vColor, 1.0);
  }
|]


