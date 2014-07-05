namespace F2048

open GameGrid

module Bot =
    
    type fct =  GameGrid.Game -> GameGrid.Move

    val random :GameGrid.Game -> GameGrid.Move

    val bit_clever :GameGrid.Game -> GameGrid.Move