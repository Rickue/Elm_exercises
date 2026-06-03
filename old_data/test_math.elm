module TestMath exposing (..)

import Statistics

testList : List Float
testList =
    [1, 2, 3, 4]

ans =
    Statistics.quantile 0.5 testList
