namespace F2048

open GameGrid

module Bot =

    type fct = GameGrid.Game -> GameGrid.Move
    let rnd = new System.Random()

    let random (_ :GameGrid.Game) = match rnd.Next(1, 5) with
                                        | 1 -> GameGrid.Left
                                        | 2 -> GameGrid.Right
                                        | 3 -> GameGrid.Up
                                        | 4 -> GameGrid.Down
                                        | _ -> GameGrid.Left
                                       
    let bit_clever (game :GameGrid.Game) :GameGrid.Move = 
        let combs (values : GameGrid.Move list) n =
            let init = List.map (fun x -> [x]) values
            [1..(n-1)]
            |>  List.fold (fun acc _ -> List.collect (fun x -> List.map (fun y -> x :: y) acc) values) init
        let applyPath game path =
            let evaluatedGame = List.fold (fun acc p -> GameGrid.move acc p) game path
            (path, evaluatedGame.score)
        let paths = combs [GameGrid.Left; GameGrid.Down; GameGrid.Up; GameGrid.Right] 5
        let best_paths = paths
                        |> List.map (applyPath game)
                        |> List.maxBy (fun (_,score) -> score)
        let p, _ = best_paths
        let rec ensureMoves p = match p with
                                | [] -> GameGrid.Left
                                | x :: xs -> if ( GameGrid.equal (GameGrid.move game x) game) then ensureMoves xs else x
        ensureMoves p


