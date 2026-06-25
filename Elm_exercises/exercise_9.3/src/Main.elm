module Main exposing (main)

-- Für die Walker-Layoutberechnung

import Browser
import Color
import Hierarchy
import Html exposing (Html, div, text)
import Html.Attributes
import Http
import Json.Decode
import Tree exposing (Tree)
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


type alias Model =
    { tree : Tree String, errorMsg : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = Tree.singleton "", errorMsg = "Loading ..." }
    , Http.get
        { url = "https://gist.githubusercontent.com/curran/1dd7ab046a4ed32380b21e81a38447aa/raw/e04346c8fa26fb1d0f3a866f6ff30ddee74f9ae6/countryHierarchy.json"
        , expect = Http.expectJson GotCountryHierarchy treeDecoder
        }
    )


type Msg
    = GotCountryHierarchy (Result Http.Error (Tree String))



--decode


treeDecoder : Json.Decode.Decoder (Tree String)
treeDecoder =
    Json.Decode.map2
        (\id children ->
            case children of
                Nothing ->
                    Tree.tree id []

                Just c ->
                    Tree.tree id c
        )
        -- Greift tief in { "data": { "id": "..." } } hinein
        (Json.Decode.at [ "data", "id" ] Json.Decode.string)
        -- Dekodiert rekursiv die Kind-Knoten, falls vorhanden
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )


type alias LayoutData =
    { x : Float, y : Float, name : String }



-- Enorm breit gewählt, da die Welt sehr viele Länder hat!


canvasWidth : Float
canvasWidth =
    4500


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
                , Hierarchy.parentChildMargin 160 -- Reichlich Platz für lange Ländernamen
                , Hierarchy.peerMargin 10
                , Hierarchy.size (canvasWidth - padding * 2) (canvasHeight - padding * 3)
                ]
                treeToLayout
    in
    Tree.map (\n -> { x = n.x, y = n.y, name = n.node }) layedOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCountryHierarchy (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotCountryHierarchy (Err error) ->
            ( { model
                | tree = Tree.singleton ""
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Fehler beim Laden der Länderdaten."
              }
            , Cmd.none
            )


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

                -- Kanten zeichnen
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

                -- Knoten mit um 90 Grad gedrehten Ländernamen zeichnen
                renderNode : LayoutData -> Svg msg
                renderNode node =
                    g []
                        [ circle
                            [ cx node.x
                            , cy node.y
                            , r 4
                            , fill (Paint (Color.rgb 0.2 0.6 0.4)) -- Schönes "Länder-Grün"
                            ]
                            []
                        , text_
                            [ transform [ Rotate 90 node.x node.y ]
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
            -- Umhüllendes SVG-Element mit horizontaler Scrollbar-Möglichkeit im Browser, falls nötig
            div [ Html.Attributes.style "overflow-x" "auto" ]
                [ svg [ viewBox 0 0 canvasWidth canvasHeight, width canvasWidth, height canvasHeight ]
                    [ g [ transform [ Translate padding padding ] ]
                        [ links
                        , nodes
                        ]
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
