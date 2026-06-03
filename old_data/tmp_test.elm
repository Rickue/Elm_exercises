import Html

type alias MultiDimPoint =
    { pointName : String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
