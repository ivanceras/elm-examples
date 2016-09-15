module Widget exposing (..)

type Msg
    = DoSomething


type alias Model =
    { name: String
    , description: String
    }

create: Model
create =
    { name = "Statistics"
    , description = "Hello"
    }
