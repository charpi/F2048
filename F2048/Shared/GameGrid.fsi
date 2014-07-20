namespace F2048

module GameGrid =
    
    type Move = Left | Right | Up | Down
    type Row = int list
    type Grid = Row list
    type Score = int
    type GameStatus = Over | Won | Running
    type Randomizer = int -> int
    type Game = {score :Score; grid :Grid; status :GameStatus; randomizer :Randomizer}

    val equal : Game -> Game -> bool
    val toString : Game -> string
    val toStringWithScore : Game -> string
    val fromStringWithScore : string -> Game
    val move : Game -> Move -> Game
    val create : Game
    val createWithRandomizer : Randomizer -> Game
    val fromArray : (int [][] -> Game)
    val fromArrayWithRandomizer : Randomizer -> int [][] -> Game
    val defaultRandomizer :Randomizer