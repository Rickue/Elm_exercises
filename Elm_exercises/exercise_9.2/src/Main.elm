module Main exposing (main)

import Browser
import Color
import Hierarchy
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Tree exposing (Tree)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



-- hierarchy Walker-Layoutberechnung


type alias Model =
    { tree : Tree String, errorMsg : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = Tree.singleton "", errorMsg = "Loading ..." }
    , Http.get { url = "../../data_csv/flare.json", expect = Http.expectJson GotFlare treeDecoder }
    )


type Msg
    = GotFlare (Result Http.Error (Tree String))


treeDecoder : Json.Decode.Decoder (Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    Tree.tree name []

                Just c ->
                    Tree.tree name c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = Tree.singleton ""
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )


type alias LayoutData =
    { x : Float, y : Float, name : String }


canvasWidth : Float
canvasWidth =
    2400


canvasHeight : Float
canvasHeight =
    900


computeLayout : Tree String -> Tree LayoutData
computeLayout treeToLayout =
    let
        padding =
            50

        layedOut =
            Hierarchy.tidy
                [ Hierarchy.nodeSize (\_ -> ( 15, 15 ))
                , Hierarchy.parentChildMargin 140 --vertikal mehr platz, da labels gedreht
                , Hierarchy.peerMargin 14
                , Hierarchy.size (canvasWidth - padding * 2) (canvasHeight - padding * 3)
                ]
                treeToLayout
    in
    Tree.map (\n -> { x = n.x, y = n.y, name = n.node }) layedOut


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "font-family" "sans-serif", Html.Attributes.style "padding" "20px" ]
        [ div [ Html.Attributes.style "margin-bottom" "10px", Html.Attributes.style "font-weight" "bold" ]
            [ Html.text ("Status: " ++ model.errorMsg) ]
        , if model.errorMsg == "No Error" then
            let
                layedOut =
                    computeLayout model.tree

                padding =
                    50

                --kanten zeichnen
                links =
                    Tree.links layedOut
                        |> List.map
                            (\( from, to ) ->
                                line
                                    [ x1 from.x
                                    , y1 from.y
                                    , x2 to.x
                                    , y2 to.y
                                    , stroke (Paint (Color.rgb 0.7 0.7 0.7))
                                    , strokeWidth (Px 1)
                                    ]
                                    []
                            )
                        |> g []

                --labels und knoten zeichnen
                renderNode : LayoutData -> Svg msg
                renderNode node =
                    g []
                        [ circle
                            [ cx node.x
                            , cy node.y
                            , r 4
                            , fill (Paint (Color.rgb 0.2 0.4 0.8))
                            ]
                            []
                        , text_
                            [ transform [ Rotate 90 node.x node.y ] --rotation 90

                            -- ausrichtung des rotierten texts
                            , x (node.x + 8)
                            , y (node.y + 3)
                            , textAnchor AnchorStart
                            , fontSize (Px 9)
                            , fontFamily [ "sans-serif" ]
                            ]
                            [ TypedSvg.Core.text node.name ]
                        ]

                nodes =
                    Tree.toList layedOut
                        |> List.map renderNode
                        |> g []
            in
            svg [ viewBox 0 0 canvasWidth canvasHeight, width canvasWidth, height canvasHeight ]
                [ g [ transform [ Translate padding padding ] ]
                    [ links
                    , nodes
                    ]
                ]

          else
            div [] []
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


labelToHtml : String -> Html msg
labelToHtml l =
    Html.text l


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]
