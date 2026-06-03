import sys

with open("src/Scatterplot2_5.elm", "r") as f:
    lines = f.readlines()

# Find where 'type CarType' starts
idx = 0
for i, line in enumerate(lines):
    if line.startswith("type CarType"):
        idx = i
        break

new_code = """module Main exposing (main)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))


w : Float
w =
    450

h : Float
h =
    450

padding : Float
padding =
    60

radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    5

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)

yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        ( min, maxim ) =
            Statistics.extent values |> Maybe.withDefault defaultExtent

        range =
            maxim - min

        extend =
            range / (2 * toFloat tickCount)
    in
    ( max 0 (min - extend), maxim + extend )

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)

yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

type alias Point =
    { pointName : String, x : Float, y : Float }

type alias PartitionedCars =
    { otherCars : List Car
    , belowAverage : List Car
    , aboveAverage : List Car
    , averageValue : Maybe Float
    }

chosenCarType : CarType
chosenCarType =
    Small_Sporty_Compact_Large_Sedan

carTypeToString : CarType -> String
carTypeToString carType =
    case carType of
        Small_Sporty_Compact_Large_Sedan -> "Small, Sporty, Compact, Large Sedan"
        Sports_Car -> "Sports Car"
        SUV -> "SUV"
        Wagon -> "Wagon"
        Minivan -> "Minivan"
        Pickup -> "Pickup"

mpgToLiterPer100km : Int -> Float
mpgToLiterPer100km mpg =
    235.215 / toFloat mpg

meanFloat : List Float -> Maybe Float
meanFloat values =
    if List.isEmpty values then
        Nothing
    else
        Just (List.sum values / toFloat (List.length values))

filterAndReduceCarsMPG : List Car -> PartitionedCars
filterAndReduceCarsMPG completeCars =
    let
        chosenCars =
            completeCars |> List.filter (\\c -> c.carType == chosenCarType)
            
        withMPG = 
            chosenCars |> List.filterMap (\\c -> Maybe.map (\\mpg -> { car = c, val = toFloat mpg }) c.cityMPG)

        averageVal =
            withMPG |> List.map .val |> meanFloat

        otherCars =
            completeCars |> List.filter (\\c -> c.carType /= chosenCarType)

        chosenPartition =
            case averageVal of
                Nothing ->
                    { belowAverage = [], aboveAverage = [] }

                Just avg ->
                    let
                        below = withMPG |> List.filter (\\entry -> entry.val < avg) |> List.map .car
                        above = withMPG |> List.filter (\\entry -> entry.val >= avg) |> List.map .car
                    in
                    { belowAverage = below, aboveAverage = above }
    in
    { otherCars = otherCars
    , belowAverage = chosenPartition.belowAverage
    , aboveAverage = chosenPartition.aboveAverage
    , averageValue = averageVal
    }

filterAndReduceCarsLiters : List Car -> PartitionedCars
filterAndReduceCarsLiters completeCars =
    let
        chosenCars =
            completeCars |> List.filter (\\c -> c.carType == chosenCarType)
            
        withLiters = 
            chosenCars |> List.filterMap (\\c -> Maybe.map (\\mpg -> { car = c, val = mpgToLiterPer100km mpg }) c.cityMPG)

        averageVal =
            withLiters |> List.map .val |> meanFloat

        otherCars =
            completeCars |> List.filter (\\c -> c.carType /= chosenCarType)

        chosenPartition =
            case averageVal of
                Nothing ->
                    { belowAverage = [], aboveAverage = [] }

                Just avg ->
                    let
                        -- WICHTIG: Weniger L/100km = Besser (= sparsam).
                        below = withLiters |> List.filter (\\entry -> entry.val < avg) |> List.map .car
                        above = withLiters |> List.filter (\\entry -> entry.val >= avg) |> List.map .car
                    in
                    { belowAverage = below, aboveAverage = above }
    in
    { otherCars = otherCars
    , belowAverage = chosenPartition.belowAverage
    , aboveAverage = chosenPartition.aboveAverage
    , averageValue = averageVal
    }

pointsFromCars : (Car -> Maybe Float) -> List Car -> List Point
pointsFromCars xMapper inputCars =
    inputCars
        |> List.filterMap
            (\\car ->
                Maybe.map2
                    (\\xval retail ->
                        { pointName = car.vehicleName
                        , x = xval
                        , y = toFloat retail
                        }
                    )
                    (xMapper car)
                    car.retailPrice
            )

renderPlot : String -> String -> (Car -> Maybe Float) -> PartitionedCars -> Svg msg
renderPlot plotClass xAxisLabel xMapper model =
    let
        otherPoints = pointsFromCars xMapper model.otherCars
        belowPoints = pointsFromCars xMapper model.belowAverage
        abovePoints = pointsFromCars xMapper model.aboveAverage

        allPoints = otherPoints ++ belowPoints ++ abovePoints

        xValues = if List.isEmpty allPoints then [ 0 ] else List.map .x allPoints
        yValues = if List.isEmpty allPoints then [ 0 ] else List.map .y allPoints

        xScaleLocal = xScale xValues
        yScaleLocal = yScale yValues
        
        labelX = wideExtent xValues |> (\\t -> (Tuple.second t - Tuple.first t) / 2)
        labelY = wideExtent yValues |> Tuple.second
        
        drawPoints pts cname color =
            g [ class [ "point", cname ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]
                (List.map
                    (\\p ->
                        circle
                            [ cx <| Scale.convert xScaleLocal p.x
                            , cy <| Scale.convert yScaleLocal p.y
                            , r radius
                            , TypedSvg.Attributes.fill (TypedSvg.Types.PaintColor (TypedSvg.Types.Rect <| TypedSvg.Types.rgba (Tuple.first color) (Tuple.second color) (Tuple.third color) (Tuple.first (Tuple.second (Tuple.second color)))))
                            ]
                            [ TypedSvg.Core.title [] [ TypedSvg.Core.text p.pointName ] ]
                    )
                    pts
                )
    in
    svg [ class [ plotClass ], viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ g [ transform [ Translate padding (h - padding) ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelX)
                , y 35
                , textAnchor AnchorMiddle
                ]
                [ TypedSvg.Core.text xAxisLabel ]
            ]
        , g [ transform [ Translate padding padding ] ]
            [ yAxis yValues
            , text_
                [ x (-((h - 2 * padding) / 2))
                , y -40
                , transform [ Rotate -90 0 0 ]
                , textAnchor AnchorMiddle
                ]
                [ TypedSvg.Core.text "Retail Price ($)" ]
            ]
        , g [ transform [ Translate padding padding ] ]
            [ -- Hacky explicit styling for demonstration
              g [] [ TypedSvg.style [] [ TypedSvg.Core.text ("""
                .""" ++ plotClass ++ """ .point circle { fill: rgba(200,200,200,0.5); }
                .""" ++ plotClass ++ """ .point.above circle { fill: rgba(46,139,87,1.0); }
                .""" ++ plotClass ++ """ .point.below circle { fill: rgba(214,39,40,1.0); }
              """) ] ]
            , drawPoints otherPoints "other" (200,200,200, (0.5, (0,0)))
            , drawPoints belowPoints "below" (214,39,40, (1.0, (0,0)))
            , drawPoints abovePoints "above" (46,139,87, (1.0, (0,0)))
            ]
        ]

roundFloat : Int -> Float -> Float
roundFloat decimals val =
    let factor = toFloat (10 ^ decimals)
    in toFloat (round (val * factor)) / factor

main : Html msg
main =
    let
        partMPG = filterAndReduceCarsMPG cars
        partLiters = filterAndReduceCarsLiters cars
    in
    Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "align-items" "center", Html.Attributes.style "font-family" "sans-serif" ]
        [ Html.h2 [] [ Html.text "Aufgabe 2.5: Das MPG vs L/100km Paradox" ]
        , Html.p [] [ Html.text ("Gewählte Klasse: " ++ carTypeToString chosenCarType) ]
        , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "20px", Html.Attributes.style "justify-content" "center", Html.Attributes.style "width" "100%" ]
            [ Html.div []
                [ Html.h3 [] [ Html.text "Meilen pro Gallone (MPG)" ]
                , Html.p [] [ Html.text ("Durchschnitt: " ++ String.fromFloat (Maybe.withDefault 0 partMPG.averageValue |> roundFloat 2) ++ " MPG") ]
                , renderPlot "plot-mpg" "City MPG" (\\c -> Maybe.map toFloat c.cityMPG) partMPG
                ]
            , Html.div []
                [ Html.h3 [] [ Html.text "Liter pro 100km" ]
                , Html.p [] [ Html.text ("Durchschnitt: " ++ String.fromFloat (Maybe.withDefault 0 partLiters.averageValue |> roundFloat 2) ++ " L/100km") ]
                , renderPlot "plot-liters" "Liters / 100km" (\\c -> Maybe.map (\\mpg -> mpgToLiterPer100km mpg) c.cityMPG) partLiters
                ]
            ]
        , Html.p [ Html.Attributes.style "max-width" "900px", Html.Attributes.style "margin" "20px auto", Html.Attributes.style "text-align" "justify" ]
            [ Html.text "Das Paradoxon tritt auf, weil MPG eine umgekehrte Metrik ist (Distanz/Volume). Ein kleiner Zuwachs bei ineffizienten Fahrzeugen spart mehr Kraftstoff als bei effizienten Fahrzeugen. Bei L/100km ist das Verhältnis linear (Volume/Distanz). Folglich verteilen sich die Fahrzeuge beim L/100km-Durchschnitt um den 'echten' linearen Verbrauchsmittelwert anders als beim MPG-Durchschnitt. Grün = überdurchschnittlich effizient, Rot = unterdurchschnittlich ineffizient." ]
        ]

"""

with open("src/Scatterplot2_5.elm", "w") as f:
    f.write(new_code)
    f.writelines(lines[idx:])
