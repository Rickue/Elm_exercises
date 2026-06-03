module TestColor exposing (..)
import Color
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types exposing (Paint(..))
test = fill <| Paint <| Color.rgb255 255 0 0
