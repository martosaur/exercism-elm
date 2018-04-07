module RobotSimulator
    exposing
        ( Bearing(..)
        , Robot
        , advance
        , defaultRobot
        , simulate
        , turnLeft
        , turnRight
        )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    Robot North { x = 0, y = 0}


turnRight : Robot -> Robot
turnRight robot =
    case robot.bearing of
        North ->
            { robot | bearing = East}
    
        East ->
            { robot | bearing = South}
        
        South ->
            { robot | bearing = West}
        
        West ->
            { robot | bearing = North}


turnLeft : Robot -> Robot
turnLeft robot =
    case robot.bearing of
        North ->
            { robot | bearing = West}
    
        West ->
            { robot | bearing = South}
        
        South ->
            { robot | bearing = East}
        
        East ->
            { robot | bearing = North}


advance : Robot -> Robot
advance robot =
    let
        x = robot.coordinates.x

        y = robot.coordinates.y
    in
        case robot.bearing of
            North ->
                { robot | coordinates = { x = x, y = y + 1 } }
        
            West ->
                { robot | coordinates = { x = x - 1, y = y } }
            
            South ->
                { robot | coordinates = { x = x, y = y - 1 } }
            
            East ->
                { robot | coordinates = { x = x + 1, y = y } }
        


simulate : String -> Robot -> Robot
simulate directions robot =
    String.toList directions
    |> List.foldl (\ c r -> case c of
        'R' ->
            turnRight r 
    
        'L' ->
            turnLeft r
        
        'A' ->
            advance r
        
        _ ->
            r) robot
