module Main exposing (main)

import Browser
import Color
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

type alias Model =
    { testTree : Tree NodeData
    , flareTree : Tree NodeData
    , errorMsg : String
    }


-- reinladen
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
        { url = "/Elm_exercises/data_csv/flare.json"
        , expect = Http.expectJson GotFlare treeDecoder
        }
    )


-- update
type Msg
    = GotFlare (Result Http.Error (Tree NodeData))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | flareTree = aggregateValues newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err _) ->
            ( { model | errorMsg = "Fehler beim Laden der Flare-Daten" }, Cmd.none )


-- decodierer
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


-- aggregation, um die Werte der Knoten zu summieren
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


-- text kürzen, wenn es nicht in das Rechteck passt
shortenText : Float -> String -> String
shortenText w name =
    let
        maxChars = Basics.floor (w / 6.5)
    in
    if String.length name > maxChars then
        String.left (Basics.max 1 (maxChars - 2)) name ++ ".."
    else
        name

--rekursion, um die Kinder zu zeichnen 
canvasWidth : Float
canvasWidth = 2400

canvasHeight : Float
canvasHeight = 900

drawTreeNode : Int -> Float -> Float -> Float -> Float -> Tree NodeData -> List (Svg msg)
drawTreeNode depth currentX currentY w h tree =
    let
        node = Tree.label tree
        children = Tree.children tree
        isLeaf = List.isEmpty children

        rectSvg =
            rect
                [ x currentX
                , y currentY
                , width w
                , height h
                , fill (Paint (Color.rgba 0 0 0 0))
                , stroke (Paint (Color.rgba 1 1 1 0.6))
                , strokeWidth (Px 2)
                ]
                []

        textSvg =
            if isLeaf && w > 40 && h > 16 then
                [ text_
                    [ x (currentX + 5)
                    , y (currentY + 13)
                    , fontSize (Px 11)
                    , fontWeight FontWeightBold  -- Fettgedruckte Schrift
                    , fill (Paint Color.white)
                    , fontFamily [ "sans-serif" ]
                    ]
                    [ TypedSvg.Core.text (shortenText w node.name) ]
                ]
            else
                []

        childrenSvgs =
            drawChildren depth currentX currentY w h node.value children
    in
    rectSvg :: textSvg ++ childrenSvgs


drawChildren : Int -> Float -> Float -> Float -> Float -> Float -> List (Tree NodeData) -> List (Svg msg)
drawChildren depth posX posY w h totalValue remChildren =
    case remChildren of
        [] ->
            []

        child :: rest ->
            let
                childNode = Tree.label child
                ratio = if totalValue <= 0 then 0 else childNode.value / totalValue
                isSplitX = modBy 2 depth == 0
            in
            if isSplitX then
                let childWidth = w * ratio
                in drawTreeNode (depth + 1) posX posY childWidth h child
                    ++ drawChildren depth (posX + childWidth) posY w h totalValue rest
            else
                let childHeight = h * ratio
                in drawTreeNode (depth + 1) posX posY w childHeight child
                    ++ drawChildren depth posX (posY + childHeight) w h totalValue rest

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

        --test baum
        , h2 [ HA.style "margin-top" "30px", HA.style "color" "#bbb" ] [ Html.text "Aufgabe 10.1: Treemap für den Test-Baum" ]
        , svg
            [ viewBox 0 0 canvasWidth canvasHeight
            , width canvasWidth
            , height canvasHeight
            , HA.style "background" "#111"
            , HA.style "margin-bottom" "50px"
            ]
            [ g [] (drawTreeNode 0 0 0 canvasWidth canvasHeight model.testTree) ]

        --flare daten
        , h2 [ HA.style "color" "#bbb" ] [ Html.text "Aufgabe 10.1: Treemap für die Flare-Daten" ]
        , if model.errorMsg == "No Error" then
            svg
                [ viewBox 0 0 canvasWidth canvasHeight
                , width canvasWidth
                , height canvasHeight
                , HA.style "background" "#111"
                ]
                [ g [] (drawTreeNode 0 0 0 canvasWidth canvasHeight model.flareTree) ]
          else if model.errorMsg == "Loading Flare JSON..." then
            div [ HA.style "padding" "40px", HA.style "font-style" "italic" ] [ Html.text "Warte auf API-Daten..." ]
          else
            div [ HA.style "padding" "40px", HA.style "color" "#ff4444" ] [ Html.text "Die Flare-Daten konnten nicht geladen werden." ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }