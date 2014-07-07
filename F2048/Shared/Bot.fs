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
        let asyncApply g path = async {
            return List.map (applyPath g) path
        }
        let paths = combs [GameGrid.Left; GameGrid.Down; GameGrid.Up; GameGrid.Right] 5
        let isFirstMoveMoving path = match path with
                                        | [] -> false
                                        | x :: xs ->  if ( GameGrid.equal (GameGrid.move game x) game) then false else true
        let rec firstMoving paths = match paths with
                                    | [] -> GameGrid.Left
                                    | x :: xs -> if (isFirstMoveMoving x) then List.head(x) else firstMoving xs
        paths
        |> List.map (fun p -> async { return (applyPath game p)})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.toList
        |> List.sortBy (fun (_,score) -> score)
        |> List.rev
        |> List.map (fun (p,_) -> p)
        |> firstMoving