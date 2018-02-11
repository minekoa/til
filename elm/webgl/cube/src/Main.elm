import Math.Vector3 exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL exposing (..)
import AnimationFrame
import Color exposing (Color)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)

{-
https://github.com/elm-community/webgl/blob/master/examples/cube.elm
を参考に。
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


view : Model -> Html Msg
view theta =
    div [] [ h1 [] [text "くるくる回るよ"]
           , WebGL.toHtml
               [ width 300
               , height 300
               , style [("display", "block")]
               ]
                 [ WebGL.entity
                       vertexShader
                       fragmentShader
                       cubeMesh
                       (uniforms <| degrees theta)
                 ]
           , p [] ["deg:" ++ (theta |> toString)  |>  text]
           , div [] [ button [onClick RotatePlus] [text "+"]
                    , button [onClick RotateMinus] [text "-"]
                    ]
           ]


------------------------------------------------------------
-- Data
------------------------------------------------------------

-- Uniform

type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }

uniforms : Float -> Uniforms
uniforms theta =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }


-- Varying

type alias Varying =
    { vColor : Vec3
    }


-- Mesh
type alias Vertex =
    { color : Vec3
    , position : Vec3
    }

cubeMesh : Mesh Vertex
cubeMesh =
        {-                  Z(T/B)                  Y (F/B)
                            ^                     ^ 
                            :                   /
                    lft(-1,1,1)               /   rft(1,1,1)
                      +------------------------+
                    / |     :             /  / |
                  /   |     :           /  /   |
                /     |     :         /  /     |
           lbt(-1,-1,1)     :       /  /       | 
            +------------------------+ rbt(1,-1,1)
            |         |     :   /    |         | 
            |         |     : /      |         | 
            |         |     O - - - -|- - - - -|- - - - - -> X (L/R) 
            |         +--------------|---------+
            |      lfb(-1,1,-1)      |       rfb(1,1,-1)
            |     /                  |       /
            |   /                    |     /
            | /                      |   /
            |                        | /
            +------------------------+ 
          lbb(-1,-1,-1)             rbb(1,-1,-1)
         -}
    let
        rft = vec3  1  1  1
        lft = vec3 -1  1  1
        lbt = vec3 -1 -1  1
        rbt = vec3  1 -1  1
        rbb = vec3  1 -1 -1
        rfb = vec3  1  1 -1
        lfb = vec3 -1  1 -1
        lbb = vec3 -1 -1 -1
    in
        [ face Color.green  rft rfb rbb rbt
        , face Color.blue   rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red    rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color = let c =  Color.toRgb rawColor in
                vec3 (toFloat c.red / 255)
                     (toFloat c.green / 255)
                     (toFloat c.blue / 255)
        vertex position = Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]


------------------------------------------------------------
-- Shaders
------------------------------------------------------------

-- Vertex Shader

vertexShader : Shader Vertex Uniforms Varying
vertexShader = [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vColor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vColor = color;
        }
|]

-- Fragment Shader

fragmentShader : Shader {} Uniforms Varying
fragmentShader = [glsl|
  precision mediump float;
  varying vec3 vColor;
  void main () {
    gl_FragColor = vec4(vColor, 1.0);
  }
|]


