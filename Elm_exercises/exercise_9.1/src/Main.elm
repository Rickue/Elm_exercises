module Main exposing (main)

import Browser
import Color
import Hierarchy
import Html exposing (Html, div, h2)
import Html.Attributes 
import Tree exposing (Tree)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))



type alias NodeData =
    { id : String }


binaryTree : Tree NodeData
binaryTree =
    Tree.tree { id = "root" }
        [ Tree.tree { id = "L" }
            [ Tree.tree { id = "LL" }
                [ Tree.tree { id = "LLL" }
                    [ Tree.singleton { id = "LLLL" } ]
                ]
            , Tree.tree { id = "LR" }
                [ Tree.tree { id = "LRL" }
                    [ Tree.singleton { id = "LRLL" } ]
                , Tree.singleton { id = "LRR" }
                ]
            ]
        , Tree.tree { id = "R" }
            [ Tree.singleton { id = "RL" }
            , Tree.tree { id = "RR" }
                [ Tree.tree { id = "RRR" }
                    [ Tree.singleton { id = "RRRR" } ]
                ]
            ]
        ]


generalTree : Tree NodeData
generalTree =
    Tree.tree { id = "root" }
        [ Tree.tree { id = "c1" }
            [ Tree.singleton { id = "c1_1" }
            , Tree.singleton { id = "c1_2" }
            , Tree.singleton { id = "c1_3" }
            , Tree.singleton { id = "c1_4" }
            , Tree.singleton { id = "c1_5" }
            , Tree.singleton { id = "c1_6" }
            , Tree.tree { id = "c1_7" }
                [ Tree.singleton { id = "c1_7_1" } ]
            ]
        , Tree.singleton { id = "c2" }
        , Tree.singleton { id = "c3" }
        , Tree.tree { id = "c4" }
            [ Tree.singleton { id = "c4_1" } ]
        , Tree.singleton { id = "c5" }
        , Tree.tree { id = "c6" }
            [ Tree.tree { id = "c6_1" }
                [ Tree.singleton { id = "c6_1_1" }
                , Tree.singleton { id = "c6_1_2" }
                , Tree.singleton { id = "c6_1_3" }
                , Tree.singleton { id = "c6_1_4" }
                , Tree.singleton { id = "c6_1_5" }
                , Tree.singleton { id = "c6_1_6" }
                , Tree.singleton { id = "c6_1_7" }
                , Tree.singleton { id = "c6_1_8" }
                ]
            ]
        ]




type alias LayoutData =
    { x : Float, y : Float }

--Walker-Algorithmus -> tidy funktion
computeLayout : Float -> Float -> Tree NodeData -> Tree LayoutData
computeLayout layoutWidth layoutHeight treeToLayout =
    Hierarchy.tidy
        [ Hierarchy.nodeSize (\_ -> ( 25, 25 ))
        --, Hierarchy.parentChildMargin 90
        --, Hierarchy.peerMargin 100
        , Hierarchy.size layoutWidth layoutHeight
        ]
        treeToLayout
        |> Tree.map (\n -> { x = n.x, y = n.y })


viewTree : Float -> Float -> Tree NodeData -> Svg msg
viewTree viewWidth viewHeight treeToRender =
    let
        padding =
            50

        layedOut =
            computeLayout (viewWidth - padding * 2) (viewHeight - padding * 2) treeToRender

        links =
            Tree.links layedOut
                |> List.map
                    (\( from, to ) ->
                        TypedSvg.line
                            [ InPx.x1 from.x
                            , InPx.y1 from.y
                            , InPx.x2 to.x
                            , InPx.y2 to.y
                            , stroke (Paint (Color.rgb 0.4 0.4 0.4))
                            , InPx.strokeWidth 2
                            ]
                            []
                    )
                |> g []

        renderNode : LayoutData -> Svg msg
        renderNode node =
            circle
                [ InPx.cx node.x
                , InPx.cy node.y
                , InPx.r 12
                , fill (Paint (Color.rgb 0.4 0.4 0.4))
                ]
                []

        nodes =
            Tree.toList layedOut
                |> List.map renderNode
                |> g []
    in
    svg [ viewBox 0 0 viewWidth viewHeight, InPx.width viewWidth, InPx.height viewHeight ]
        [ g [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
            [ links
            , nodes
            ]
        ]


-- 4. ANZEIGE-BOARD

boardWidth : Float
boardWidth =
    800


boardHeight : Float
boardHeight =
    450


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view =
            \_ ->
                div [ Html.Attributes.style "font-family" "sans-serif", Html.Attributes.style "padding" "20px" ] -- KORREKTUR: Großgeschriebenes Modul verwendet
                    [ h2 [] [ Html.text "Aufgabe 9.1 (a): Binärer Baum (Folie 405)" ]
                    , viewTree boardWidth boardHeight binaryTree
                    , h2 [] [ Html.text "Aufgabe 9.1 (b): Allgemeiner geordneter Baum (Folie 412)" ]
                    , viewTree boardWidth boardHeight generalTree
                    ]
        }