namespace F2048

module GameGrid =
    
    type Move = Left | Right | Up | Down
    type Row = int list
    type Grid = Row list
    type Score = int
    type Game = { score :Score ; grid :Grid}

    val equal : Game -> Game -> bool
    val toString : Game -> string
    val move : Game -> Move -> Game
    val create : unit -> Game