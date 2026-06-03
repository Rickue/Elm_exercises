module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Color
import Csv
import Csv.Decode
import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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


-- INDEX CONFIGURATION

type Index
    = DJ
    | DAX
    | NIKKEI
    | HANGSENG
    | BOVESPA


indexConfig : Index -> { url : String, start : String, name : String }
indexConfig idx =
    case idx of
        DJ -> { url = "../data_csv/DJ.csv", start = "1980-12-23", name = "Dow Jones" }
        DAX -> { url = "../data_csv/DAX.csv", start = "1990-11-26", name = "DAX" }
        NIKKEI -> { url = "../data_csv/NIKKEI.csv", start = "1984-01-04", name = "Nikkei" }
        HANGSENG -> { url = "../data_csv/HANGSENG.csv", start = "1986-12-31", name = "Hang Seng" }
        BOVESPA -> { url = "../data_csv/BOVESPA.csv", start = "1993-04-27", name = "Bovespa" }


-- MODEL

type alias Series =
    List ( String, Maybe Float )


type alias CurrentRecordedData msg =
    RecordedData ( String, Maybe Float ) (List (TypedSvg.Core.Attribute msg))


type Model
    = Failure Index
    | Loading Index
    | Success
        { activeIndex : Index
        , raw : Series
        , expanded : Series
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading DJ
    , fetchIndexCmd DJ
    )


fetchIndexCmd : Index -> Cmd Msg
fetchIndexCmd idx =
    Http.get
        { url = (indexConfig idx).url
        , expect = Http.expectString (GotText idx)
        }


-- UPDATE

type Msg
    = SelectIndex Index
    | GotText Index (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectIndex idx ->
            ( Loading idx
            , fetchIndexCmd idx
            )

        GotText idx result ->
            case result of
                Ok fullText ->
                    let
                        rawSeries =
                            csvString_to_data fullText

                        expandedSeries =
                            expandSeries idx rawSeries
                    in
                    ( Success
                        { activeIndex = idx
                        , raw = rawSeries
                        , expanded = expandedSeries
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( Failure idx, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    let
        renderButtons currentIdx =
            div [ style "margin-bottom" "20px", style "display" "flex", style "gap" "10px" ]
                [ viewButton DJ "Dow Jones" currentIdx
                , viewButton DAX "DAX" currentIdx
                , viewButton NIKKEI "Nikkei" currentIdx
                , viewButton HANGSENG "Hang Seng" currentIdx
                , viewButton BOVESPA "Bovespa" currentIdx
                ]

        viewButton idx label currentIdx =
            let
                isActive =
                    idx == currentIdx
            in
            button
                [ onClick (SelectIndex idx)
                , style "padding" "10px 16px"
                , style "font-size" "14px"
                , style "font-weight" "bold"
                , style "cursor" "pointer"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "background-color" (if isActive then "#39ff14" else "#444")
                , style "color" (if isActive then "#000" else "#fff")
                ]
                [ text label ]
    in
    div [ style "background-color" "#222", style "color" "#fff", style "padding" "20px", style "min-height" "100vh", style "font-family" "sans-serif" ]
        [ case model of
            Failure idx ->
                div []
                    [ renderButtons idx
                    , text ("Fehler beim Laden der CSV-Daten für: " ++ (indexConfig idx).name)
                    ]

            Loading idx ->
                div []
                    [ renderButtons idx
                    , div [ style "font-size" "18px", style "margin-top" "20px" ] [ text ("Lade " ++ (indexConfig idx).name ++ " Daten...") ]
                    ]

            Success data ->
                let
                    config =
                        indexConfig data.activeIndex

                    level =
                        [ Level 6 6   -- Außen: Jahre
                        , Level 4 3   -- Monate pro Jahr
                        , Level 1 5   -- Wochen pro Monat
                        , Level 7 1   -- Innen: 7 Wochentage nebeneinander
                        ]

                    ( w, h ) =
                        ( 840, 450 )

                    currentValues : List Float
                    currentValues =
                        data.expanded
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
                        List.map2 (\a b -> RecordedData a b []) pixelList data.expanded

                    drawPosition : CurrentRecordedData Msg -> CurrentRecordedData Msg
                    drawPosition (RecordedData pixelPosition value _) =
                        RecordedData
                            pixelPosition
                            value
                            (drawTuplePosition ( w, h ) level pixelPosition)

                    -- 🛠️ HIER WURDEN DIE BORDERS OPTIMIERT
                    createStyle : ( String, Maybe Float ) -> List (TypedSvg.Core.Attribute Msg)
                    createStyle ( dateString, value ) =
                        let
                            colorValue =
                                case value of
                                    Just v ->
                                        Scale.Color.viridisInterpolator (Scale.convert normalize v)

                                    Nothing ->
                                        Color.rgb255 50 50 50 -- Farbe für geschlossene Tage
                        in
                        [ TypedSvg.Attributes.title
                            (if String.isEmpty dateString then
                                "Kein Datum"
                             else
                                dateString ++ (case value of
                                                  Just v -> ": " ++ String.fromFloat v
                                                  Nothing -> ": Geschlossen"
                                              )
                            )
                        , TypedSvg.Attributes.fill (TypedSvg.Types.Paint colorValue)
                        -- 1. Farbe exakt an den Hintergrund (#222 -> RGB 34 34 34) angepasst:
                        , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint (Color.rgb255 34 34 34))
                        -- 2. Auf exakt 1px erhöht, um mathematische Rundungsfehler im Browser zu eliminieren:
                        , TypedSvg.Attributes.strokeWidth (TypedSvg.Types.px 1)
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
                in
                div []
                    [ renderButtons data.activeIndex
                    
                    , TypedSvg.svg
                        [ TypedSvg.Attributes.viewBox 0 0 840 550
                        , TypedSvg.Attributes.width (TypedSvg.Types.px 1000)
                        , TypedSvg.Attributes.height (TypedSvg.Types.px 650)
                        ]
                        [ TypedSvg.text_
                            [ TypedSvg.Attributes.x (TypedSvg.Types.px 8)
                            , TypedSvg.Attributes.y (TypedSvg.Types.px 25)
                            , TypedSvg.Attributes.fontSize (TypedSvg.Types.px 22)
                            , TypedSvg.Attributes.fontWeight TypedSvg.Types.FontWeightBold
                            , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.white)
                            ]
                            [ TypedSvg.Core.text (config.name ++ " Zeitreihe (" ++ config.start ++ " bis 2011-06-09)") ]
                        
                        , TypedSvg.g
                            [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate 0 40 ] ]
                            (List.map (drawPosition >> drawStyle >> draw) ourData)
                        
                        , TypedSvg.g
                            [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate 0 510 ] ]
                            [ TypedSvg.rect [ TypedSvg.Attributes.x (TypedSvg.Types.px 8), TypedSvg.Attributes.width (TypedSvg.Types.px 20), TypedSvg.Attributes.height (TypedSvg.Types.px 10), TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Scale.Color.viridisInterpolator 0)) ] []
                            , TypedSvg.rect [ TypedSvg.Attributes.x (TypedSvg.Types.px 33), TypedSvg.Attributes.width (TypedSvg.Types.px 20), TypedSvg.Attributes.height (TypedSvg.Types.px 10), TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Scale.Color.viridisInterpolator 0.5)) ] []
                            , TypedSvg.rect [ TypedSvg.Attributes.x (TypedSvg.Types.px 58), TypedSvg.Attributes.width (TypedSvg.Types.px 20), TypedSvg.Attributes.height (TypedSvg.Types.px 10), TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Scale.Color.viridisInterpolator 1)) ] []
                            , TypedSvg.rect [ TypedSvg.Attributes.x (TypedSvg.Types.px 83), TypedSvg.Attributes.width (TypedSvg.Types.px 20), TypedSvg.Attributes.height (TypedSvg.Types.px 10), TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Color.rgb255 50 50 50)) ] []
                            
                            , TypedSvg.text_
                                [ TypedSvg.Attributes.x (TypedSvg.Types.px 115)
                                , TypedSvg.Attributes.y (TypedSvg.Types.px 10)
                                , TypedSvg.Attributes.fontSize (TypedSvg.Types.px 12)
                                , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.white)
                                ]
                                [ TypedSvg.Core.text ("Index-Wert: Dunkellila (Min: " ++ String.fromFloat minValue ++ ") → Neongelb (Max: " ++ String.fromFloat maxValue ++ ") | Grau = Börse geschlossen") ]
                            ]
                        ]
                    ]
        ]


csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


expandSeries : Index -> Series -> Series
expandSeries idx series =
    Dict.union (Dict.fromList series) (holidayDict idx)
        |> Dict.toList


holidayDict : Index -> Dict String (Maybe Float)
holidayDict idx =
    Date.range Date.Day 1 (startDate idx) endDate
        |> List.map (\dateValue -> ( Date.toIsoString dateValue, Nothing ))
        |> Dict.fromList


startDate : Index -> Date
startDate idx =
    Date.fromIsoString (indexConfig idx).start
        |> Result.withDefault (Date.fromRataDie 0)


endDate : Date
endDate =
    Date.fromIsoString "2011-06-09"
        |> Result.withDefault (Date.fromRataDie 0)


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\date open -> ( date, Just open ))
        (Csv.Decode.field "Date" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "Open"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                )
        )