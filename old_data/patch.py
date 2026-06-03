import re

with open("src/Scatterplot2_5.elm", "r") as f:
    content = f.read()

# add conversion function before filterAndReduceCars
func = """
mpgToLiterPer100km : Float -> Float
mpgToLiterPer100km mpg =
    235.214583 / mpg
"""
content = function_insertion = content.replace("filterAndReduceCars : List Car -> PartitionedCars", func + "\n\nfilterAndReduceCars : List Car -> PartitionedCars")

# update pointsFromCars to accept a conversion function
content = content.replace("pointsFromCars : List Car -> List Point\npointsFromCars inputCars =\n    inputCars\n        |> List.filterMap\n            (\lambda car ->\n                Maybe.map2\n                    (\lambda city retail ->\n                        { pointName = car.vehicleName\n                        , x = toFloat city\n                        , y = toFloat retail\n                        }\n                    )\n                    car.cityMPG\n                    car.retailPrice\n            )", 
"""pointsFromCars : (Float -> Float) -> List Car -> List Point
pointsFromCars convX inputCars =
    inputCars
        |> List.filterMap
            (\\car ->
                Maybe.map2
                    (\\city retail ->
                        { pointName = car.vehicleName
                        , x = convX (toFloat city)
                        , y = toFloat retail
                        }
                    )
                    car.cityMPG
                    car.retailPrice
            )""")

content = content.replace("pointsFromCars model.otherCars", "pointsFromCars identity model.otherCars")
content = content.replace("pointsFromCars model.belowAverage", "pointsFromCars identity model.belowAverage")
content = content.replace("pointsFromCars model.aboveAverage", "pointsFromCars identity model.aboveAverage")

# create a secondary plotValues function and scatterplot for liters
content = content.replace("scatterplotV1 : PartitionedCars -> Svg msg", """plotValuesLiters : PartitionedCars -> { otherPoints : List Point, belowPoints : List Point, abovePoints : List Point, xValues : List Float, yValues : List Float, xScaleLocal : ContinuousScale Float, yScaleLocal : ContinuousScale Float, labelX : Float, labelY : Float }
plotValuesLiters model =
    let
        otherPoints = pointsFromCars mpgToLiterPer100km model.otherCars
        belowPoints = pointsFromCars mpgToLiterPer100km model.belowAverage
        abovePoints = pointsFromCars mpgToLiterPer100km model.aboveAverage
        allPoints = otherPoints ++ belowPoints ++ abovePoints

        xValues = if List.isEmpty allPoints then [ 0 ] else List.map .x allPoints
        yValues = if List.isEmpty allPoints then [ 0 ] else List.map .y allPoints
        xScaleLocal = xScale xValues
        yScaleLocal = yScale yValues
    in
    { otherPoints = otherPoints, belowPoints = belowPoints, abovePoints = abovePoints
    , xValues = xValues, yValues = yValues
    , xScaleLocal = xScaleLocal, yScaleLocal = yScaleLocal
    , labelX = wideExtent xValues |> (\\t -> (Tuple.second t - Tuple.first t) / 2)
    , labelY = wideExtent yValues |> Tuple.second
    }

renderPlotLiters : String -> String -> (ContinuousScale Float -> ContinuousScale Float -> String -> Point -> Svg msg) -> PartitionedCars -> Svg msg
renderPlotLiters plotClass extraCss drawPoint model =
    let
        p = plotValuesLiters model
    in
    svg [ class [ plotClass ], viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text ("""\"\"\"
            .""" ++ plotClass ++ """ .point.other circle, .""" ++ plotClass ++ """ .point.other rect, .""" ++ plotClass ++ """ .point.other polygon { fill: rgb(240, 240, 240); }
            .""" ++ plotClass ++ """ .point text { display: none; }
            .""" ++ plotClass ++ """ .point:hover circle, .""" ++ plotClass ++ """ .point:hover rect, .""" ++ plotClass ++ """ .point:hover polygon { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .""" ++ plotClass ++ """ .point:hover text { display: inline; }
          \"\"\"""" ++ extraCss) ]
        , g [ transform [ Translate padding (h - padding) ] ]
            [ xAxis p.xValues
            , text_ [ x (Scale.convert p.xScaleLocal p.labelX), y 30, textAnchor AnchorMiddle ] [ TypedSvg.Core.text "Liters / 100km" ]
            ]
        , g [ transform [ Translate padding padding ] ]
            [ yAxis p.yValues
            , text_ [ x 0, y (Scale.convert p.yScaleLocal p.labelY - 20), textAnchor AnchorMiddle ] [ TypedSvg.Core.text "retailPrice" ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.concat
                [ List.map (drawPoint p.xScaleLocal p.yScaleLocal "other") p.otherPoints
                , List.map (drawPoint p.xScaleLocal p.yScaleLocal "above") p.abovePoints -- NOTE: below/above is inverted conceptually for MPG vs L/100km
                , List.map (drawPoint p.xScaleLocal p.yScaleLocal "below") p.belowPoints
                ]
            )
        ]

scatterplotLiters : PartitionedCars -> Svg msg
scatterplotLiters model =
    renderPlotLiters
        "plot-liters"
        ".plot-liters .point.above circle {stroke: rgba(0, 0, 0,0.4); fill: rgb(200, 200, 200); }\\n .plot-liters .point.other circle {stroke: rgba(0, 0, 0,0.4); fill: rgb(240, 240, 240); }\\n .plot-liters .point.below circle {stroke: rgba(0, 0, 0,0.4); fill: rgb(46, 139, 87); }"
        pointCircle
        model

scatterplotV1 : PartitionedCars -> Svg msg""")


c_main1 = """        , Html.h3 [] [ Html.text "Version 1: Farbe" ]
        , scatterplotV1 filteredCars
        , Html.h3 [] [ Html.text "Version 2: Form (+Farbe)" ]
        , scatterplotV2 filteredCars
        , Html.h3 [] [ Html.text "Version 3: Größe" ]
        , scatterplotV3 filteredCars"""

c_main2 = """        , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row", Html.Attributes.style "width" "100%" ]
            [ Html.div [ Html.Attributes.style "flex" "1" ]
                [ Html.h3 [] [ Html.text "Original: cityMPG vs retailPrice" ]
                , scatterplotV1 filteredCars 
                ]
            , Html.div [ Html.Attributes.style "flex" "1" ]
                [ Html.h3 [] [ Html.text "Umrechnung: L/100km vs retailPrice (Paradox)" ]
                , scatterplotLiters filteredCars 
                ]
            ]"""

content = content.replace("import Html exposing (Html)", "import Html exposing (Html)\nimport Html.Attributes")
content = content.replace(c_main1, c_main2)

with open("src/Scatterplot2_5.elm", "w") as f:
    f.write(content)
