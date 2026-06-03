module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Color
import Csv
import Csv.Decode
import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, div, pre, text)
import Http
import RecursivePattern exposing (Level(..), PixelPositon(..), RecordedData(..), augementLevel, createPixelMap, startPosition)
import RecursivePattern.Helper exposing (drawTuplePosition)
import Scale
import Scale.Color
import Statistics
import String
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Core
import TypedSvg.Types



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


useEllie : Bool
useEllie =
    False


dataUrl : String
dataUrl =
    if useEllie then
        "https://cors-anywhere.herokuapp.com/https://users.informatik.uni-halle.de/~hinnebur/Lehre/InfoVis/U06/DJ.csv"

    else
        "../data_csv/DJ.csv"


type alias Series =
    List ( String, Maybe Float )


type alias CurrentRecordedData msg =
    RecordedData ( String, Maybe Float ) (List (TypedSvg.Core.Attribute msg))


type Model
    = Failure
    | Loading
    | Success
        { raw : Series
        , expanded : Series
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = dataUrl
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    let
                        rawSeries =
                            csvString_to_data fullText

                        expandedSeries =
                            expandSeries rawSeries
                    in
                    ( Success
                        { raw = rawSeries
                        , expanded = expandedSeries
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load DJ.csv."

        Loading ->
            text "Loading..."

        Success series ->
            let
                level =
                    [ Level 5 1
                    , Level 1 12
                    , Level 4 1
                    , Level 2 3
                    , Level 3 3
                    , Level 1 1
                    ]

                ( w, h ) =
                    ( 500, 500 )

                currentValues : List Float
                currentValues =
                    series.expanded
                        |> List.filterMap Tuple.second

                normalize =
                    currentValues
                        |> Statistics.extent
                        |> Maybe.withDefault ( 0, 1 )
                        |> Scale.linear ( 0, 1 )

                ( minValue, maxValue ) =
                    currentValues
                        |> Statistics.extent
                        |> Maybe.withDefault ( 0, 1 )

                pixelList : List PixelPositon
                pixelList =
                    createPixelMap startPosition (augementLevel level)

                ourData : List (CurrentRecordedData Msg)
                ourData =
                    List.map2 (\a b -> RecordedData a b []) pixelList series.expanded

                drawPosition : CurrentRecordedData Msg -> CurrentRecordedData Msg
                drawPosition (RecordedData pixelPosition value _) =
                    RecordedData
                        pixelPosition
                        value
                        (drawTuplePosition ( w, h ) level pixelPosition)

                createStyle : ( String, Maybe Float ) -> List (TypedSvg.Core.Attribute Msg)
                createStyle ( dateString, value ) =
                    let
                        colorValue =
                            case value of
                                Just v ->
                                    Scale.Color.redYellowGreenInterpolator (1 - Scale.convert normalize v)

                                Nothing ->
                                    Color.lightGray
                    in
                    [ TypedSvg.Attributes.title
                        (if String.isEmpty dateString then
                            "Missing date"

                         else
                            dateString
                        )
                    , TypedSvg.Attributes.fill (TypedSvg.Types.Paint colorValue)
                    , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint (Color.rgb255 244 244 244))
                    , TypedSvg.Attributes.strokeWidth (TypedSvg.Types.px 0.35)
                    , TypedSvg.Attributes.shapeRendering TypedSvg.Types.RenderCrispEdges
                    ]

                drawStyle : CurrentRecordedData Msg -> CurrentRecordedData Msg
                drawStyle (RecordedData pixelPosition value attributeList) =
                    RecordedData
                        pixelPosition
                        value
                        (List.append attributeList (createStyle value))

                draw : CurrentRecordedData Msg -> TypedSvg.Core.Svg Msg
                draw (RecordedData _ _ attributeList) =
                    TypedSvg.rect
                        attributeList
                        []

                entryToLine : CurrentRecordedData msg -> String
                entryToLine (RecordedData pixelPos ( dateString, maybeValue ) _) =
                    let
                        posText =
                            case pixelPos of
                                PixelPositon x y ->
                                    "position " ++ String.fromInt x ++ ", " ++ String.fromInt y

                        valueText =
                            case maybeValue of
                                Just v ->
                                    String.fromFloat v

                                Nothing ->
                                    "Nothing"
                    in
                    dateString ++ ", " ++ valueText ++ " to " ++ posText
            in
            div []
                [ TypedSvg.svg
                    [ TypedSvg.Attributes.viewBox 0 0 500 590
                    , TypedSvg.Attributes.width (TypedSvg.Types.px 800)
                    , TypedSvg.Attributes.height (TypedSvg.Types.px 944)
                    , TypedSvg.Attributes.preserveAspectRatio
                        (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin)
                        TypedSvg.Types.Meet
                    ]
                    [ TypedSvg.text_
                        [ TypedSvg.Attributes.x (TypedSvg.Types.px 8)
                        , TypedSvg.Attributes.y (TypedSvg.Types.px 18)
                        , TypedSvg.Attributes.fontSize (TypedSvg.Types.px 16)
                        , TypedSvg.Attributes.fontWeight TypedSvg.Types.FontWeightBold
                        , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.black)
                        ]
                        [ TypedSvg.Core.text
                            ("Loaded: DJ(" ++ String.fromInt (List.length series.raw) ++ ")")
                        ]
                    , TypedSvg.g
                        [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate 0 76 ] ]
                        (List.map (drawPosition >> drawStyle >> draw) ourData)
                    , TypedSvg.g
                        [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate 0 50 ] ]
                        [ TypedSvg.rect
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 8)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 0)
                            , TypedSvg.Attributes.width (TypedSvg.Types.px 18)
                            , TypedSvg.Attributes.height (TypedSvg.Types.px 6)
                            , TypedSvg.Attributes.fill
                                (TypedSvg.Types.Paint (Scale.Color.redYellowGreenInterpolator 1))
                            ]
                            []
                        , TypedSvg.rect
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 28)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 0)
                            , TypedSvg.Attributes.width (TypedSvg.Types.px 18)
                            , TypedSvg.Attributes.height (TypedSvg.Types.px 6)
                            , TypedSvg.Attributes.fill
                                (TypedSvg.Types.Paint (Scale.Color.redYellowGreenInterpolator 0.5))
                            ]
                            []
                        , TypedSvg.rect
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 48)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 0)
                            , TypedSvg.Attributes.width (TypedSvg.Types.px 18)
                            , TypedSvg.Attributes.height (TypedSvg.Types.px 6)
                            , TypedSvg.Attributes.fill
                                (TypedSvg.Types.Paint (Scale.Color.redYellowGreenInterpolator 0))
                            ]
                            []
                        , TypedSvg.text_
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 72)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 6)
                            , TypedSvg.Attributes.fontSize (TypedSvg.Types.px 10)
                            , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.black)
                            ]
                            [ TypedSvg.Core.text
                                ("Open (index value): min "
                                    ++ String.fromFloat minValue
                                    ++ "  max "
                                    ++ String.fromFloat maxValue
                                )
                            ]
                        , TypedSvg.text_
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 72)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 18)
                            , TypedSvg.Attributes.fontSize (TypedSvg.Types.px 10)
                            , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.black)
                            ]
                            [ TypedSvg.Core.text "Grün = low, Rot = high, Grau = closed" ]
                        ]
                    ]
                , pre []
                    [ text
                        (ourData
                            |> List.map entryToLine
                            |> String.join "\n"
                        )
                    ]
                ]


csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


expandSeries : Series -> Series
expandSeries series =
    Dict.union (Dict.fromList series) holidayDict
        |> Dict.toList


holidayDict : Dict String (Maybe Float)
holidayDict =
    Date.range Date.Day 1 startDate endDate
        |> List.map (\dateValue -> ( Date.toIsoString dateValue, Nothing ))
        |> Dict.fromList


startDate : Date
startDate =
    Date.fromIsoString "1980-12-23"
        |> Result.withDefault (Date.fromRataDie 0)


endDate : Date
endDate =
    Date.fromIsoString "2011-06-09"
        |> Result.withDefault (Date.fromRataDie 0)


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\a b -> ( a, Just b ))
        (Csv.Decode.field "Date" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "Open"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                )
        )


formatEntry : ( String, Maybe Float ) -> String
formatEntry ( date, openValue ) =
    let
        dateText =
            if String.isEmpty date then
                "Missing date"

            else
                date

        openText =
            case openValue of
                Just value ->
                    String.fromFloat value

                Nothing ->
                    "Nothing"
    in
    dateText ++ ": " ++ openText
