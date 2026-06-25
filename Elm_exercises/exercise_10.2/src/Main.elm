module Main exposing (main)

import Browser
import Color
import Hierarchy
import Html exposing (Html, div, h2)
import Html.Attributes as HA
import Http
import Json.Decode
import Tree exposing (Tree)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (fill, fontFamily, fontSize, fontWeight, stroke, strokeWidth, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Paint(..))


type alias NodeData =
    { name : String
    , value : Float
    }

type alias LayoutNode =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , value : Float
    , node : NodeData
    }

type alias Model =
    { testTree : Tree NodeData
    , flareTree : Tree NodeData
    , errorMsg : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        tTree =
            aggregateValues <|
                Tree.tree { name = "Root", value = 0 }
                    [ Tree.tree { name = "A", value = 30 } []
                    , Tree.tree { name = "B", value = 0 }
                        [ Tree.tree { name = "B1", value = 20 } []
                        , Tree.tree { name = "B2", value = 50 } []
                        ]
                    ]
    in
    ( { testTree = tTree
      , flareTree = Tree.singleton { name = "Lade Daten...", value = 0 }
      , errorMsg = "Loading Flare JSON..."
      }
    , Http.get
        { url = "../../data_csv/flare.json"
        , expect = Http.expectJson GotFlare treeDecoder
        }
    )

type Msg
    = GotFlare (Result Http.Error (Tree NodeData))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | flareTree = aggregateValues newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err _) ->
            ( { model | errorMsg = "Fehler beim Laden der Flare-Daten" }, Cmd.none )


-- decoder
treeDecoder : Json.Decode.Decoder (Tree NodeData)
treeDecoder =
    Json.Decode.map3
        (\name value children ->
            case children of
                Nothing ->
                    Tree.tree { name = name, value = Maybe.withDefault 0 value } []

                Just c ->
                    Tree.tree { name = name, value = Maybe.withDefault 0 value } c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "value" Json.Decode.float))
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy (\_ -> treeDecoder)
        )


--aggregation: summieren der Werte der Knoten
aggregateValues : Tree NodeData -> Tree NodeData
aggregateValues tree =
    let
        node = Tree.label tree
        children = Tree.children tree
        aggregatedChildren = List.map aggregateValues children

        totalValue =
            if List.isEmpty aggregatedChildren then
                node.value
            else
                List.sum (List.map (\c -> (Tree.label c).value) aggregatedChildren)
    in
    Tree.tree { name = node.name, value = totalValue } aggregatedChildren

canvasWidth : Float
canvasWidth = 2400

canvasHeight : Float
canvasHeight = 900

computeTreemap : Tree NodeData -> Tree LayoutNode
computeTreemap treeToLayout =
    Hierarchy.treemap
        [ Hierarchy.size canvasWidth canvasHeight
        , Hierarchy.tile Hierarchy.squarify
        ]
        .value
        treeToLayout


--farbpalette für Ebene 1
palette : Int -> Color.Color
palette idx =
    case modBy 6 idx of
        0 -> Color.rgb 0.12 0.47 0.71
        1 -> Color.rgb 0.17 0.63 0.17
        2 -> Color.rgb 0.84 0.15 0.16
        3 -> Color.rgb 0.58 0.40 0.74
        4 -> Color.rgb 1.00 0.50 0.05
        _ -> Color.rgb 0.12 0.65 0.60


--ab ebene 2 farbmodifikation: hue shift + lightness variation
modifyColor : Color.Color -> Int -> Color.Color
modifyColor baseColor idx =
    let
        hsl = Color.toHsla baseColor
        newHue = hsl.hue + (toFloat idx * 0.05)
        wrappedHue = newHue - toFloat (floor newHue)
        newLightness =
            if modBy 2 idx == 0 then
                Basics.max 0.2 (hsl.lightness - 0.1)
            else
                Basics.min 0.8 (hsl.lightness + 0.1)
    in
    Color.hsla wrappedHue hsl.saturation newLightness hsl.alpha


--textkürzung, wenn es nicht in das Rechteck passt
shortenText : Float -> String -> String
shortenText w name =
    let
        maxChars = Basics.floor (w / 6.5)
    in
    if String.length name > maxChars then
        String.left (Basics.max 1 (maxChars - 2)) name ++ ".."
    else
        name


--rekursives rendern der Knoten
renderTreemapNodes : Int -> Int -> Color.Color -> Tree LayoutNode -> List (Svg msg)
renderTreemapNodes depth childIndex parentColor tree =
    let
        layout = Tree.label tree
        children = Tree.children tree
        isLeaf = List.isEmpty children

        --Farbe anhand der dynamisch mitgereichten Tiefe (depth)
        nodeColor =
            if depth == 0 then
                Color.rgba 0 0 0 0 -- Wurzel bleibt transparent
            else if depth == 1 then
                palette childIndex -- Ebene 1 kriegt Primärfarbe
            
            else if depth == 2 then
                modifyColor parentColor childIndex -- Ebene 2 berechnet Farbnuance
            
            else
                parentColor -- Tiefere Ebenen erben die Nuance

        rectSvg =
            rect
                [ x layout.x
                , y layout.y
                , width layout.width
                , height layout.height
                , fill (Paint (if isLeaf then nodeColor else Color.rgba 0 0 0 0))
                , stroke (Paint (Color.rgba 1 1 1 0.6))
                , strokeWidth (Px 2)
                ]
                []

        textSvg =
            if isLeaf && layout.width > 40 && layout.height > 16 then
                [ text_
                    [ x (layout.x + 5)
                    , y (layout.y + 13)
                    , fontSize (Px 11)
                    , fontWeight FontWeightBold
                    , fill (Paint Color.white)
                    , fontFamily [ "sans-serif" ]
                    ]
                    [ TypedSvg.Core.text (shortenText layout.width layout.node.name) ]
                ]
            else
                []

        childrenSvgs =
            children
                |> List.indexedMap (\idx child -> renderTreemapNodes (depth + 1) idx nodeColor child)
                |> List.concat
    in
    rectSvg :: textSvg ++ childrenSvgs


view : Model -> Html Msg
view model =
    div
        [ HA.style "background-color" "#222"
        , HA.style "color" "#fff"
        , HA.style "padding" "20px"
        , HA.style "min-height" "100vh"
        , HA.style "font-family" "sans-serif"
        ]
        [ div
            [ HA.style "margin-bottom" "20px"
            , HA.style "font-weight" "bold"
            , HA.style "font-size" "18px"
            ]
            [ Html.text ("Status der API: " ++ model.errorMsg) ]


        --squarified treemap für die flare-daten
        , h2 [ HA.style "color" "#bbb" ] [ Html.text "Aufgabe 10.2: Squarified Treemap für die Flare-Daten" ]
        , if model.errorMsg == "No Error" then
            svg
                [ viewBox 0 0 canvasWidth canvasHeight
                , width canvasWidth
                , height canvasHeight
                , HA.style "background" "#111"
                ]
                [ g [] (renderTreemapNodes 0 0 Color.black (computeTreemap model.flareTree)) ]
          else if model.errorMsg == "Loading Flare JSON..." then
            div [ HA.style "padding" "40px", HA.style "font-style" "italic" ] [ Html.text "Warte auf API-Daten..." ]
          else
            div [ HA.style "padding" "40px", HA.style "color" "#ff4444" ] [ Html.text "Die Flare-Daten konnten nicht geladen werden." ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }