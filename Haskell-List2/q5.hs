data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)
                 
north_direction:: Direction->Command->Direction
north_direction (North)(Forward n) = North
north_direction (North)(Backward n) = North
north_direction (North)(TurnLeft) = West
north_direction (North)(TurnRight) = East

south_direction:: Direction->Command->Direction
south_direction (South)(Forward n) = South
south_direction (South)(Backward n) = South
south_direction (South)(TurnLeft) = East
south_direction (South)(TurnRight) = West

west_direction:: Direction->Command->Direction
west_direction (West)(Forward n) = West
west_direction (West)(Backward n) = West
west_direction (West)(TurnLeft) = South
west_direction (West)(TurnRight) = North

east_direction:: Direction->Command->Direction
east_direction (East)(Forward n) = East
east_direction (East)(Backward n) = East
east_direction (East)(TurnLeft) = North
east_direction (East)(TurnRight) = South
                 
faces :: Direction -> [Command] -> Direction
faces (North)([]) = North
faces (South)([]) = South
faces (East)([])  = East
faces (West)([])  = West
faces (North)(Forward n:end) = faces(north_direction(North)(Forward n))(end)
faces (North)(Backward n:end) = faces(north_direction(North)(Backward n))(end)
faces (North)(TurnLeft:end) = faces(north_direction(North)(TurnLeft))(end)
faces (North)(TurnRight:end) = faces(north_direction(North)(TurnRight ))(end)
faces (South)(Forward n:end) = faces(south_direction(South)(Forward n))(end)
faces (South)(Backward n:end) = faces(south_direction(South)(Backward n))(end)
faces (South)(TurnLeft:end) = faces(south_direction(South)(TurnLeft))(end)
faces (South)(TurnRight:end) = faces(south_direction(South)(TurnRight ))(end)
faces (West)(Forward n:end) = faces(west_direction(West)(Forward n))(end)
faces (West)(Backward n:end) = faces(west_direction(West)(Backward n))(end)
faces (West)(TurnLeft:end) = faces(west_direction(West)(TurnLeft))(end)
faces (West)(TurnRight:end) = faces(west_direction(West)(TurnRight ))(end)
faces (East)(Forward n:end) = faces(east_direction(East)(Forward n))(end)
faces (East)(Backward n:end) = faces(east_direction(East)(Backward n))(end)
faces (East)(TurnLeft:end) = faces(east_direction(East)(TurnLeft))(end)
faces (East)(TurnRight:end) = faces(east_direction(East)(TurnRight ))(end)

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result
