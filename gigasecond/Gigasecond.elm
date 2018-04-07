module Gigasecond exposing (add)

import Date

add : Date.Date -> Date.Date
add date =
    Date.toTime date
    |> (+) 1000000000000
    |> Date.fromTime