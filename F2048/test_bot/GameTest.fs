namespace F2048
open System
open NUnit.Framework

open GameGrid

[<TestFixture>]
type GameTest() = 

    let assertGrid move expectedGrid grid =
        let fakeRandom = (fun x -> 0)
        let game = GameGrid.fromArrayWithRandomizer fakeRandom grid
        let updatedGame = GameGrid.move game move
        Assert.AreEqual(4, updatedGame.score)
        Assert.AreEqual(GameGrid.Running, updatedGame.status)
        Assert.AreEqual(expectedGrid, updatedGame.grid)

    [<Test>]
    member x.Create() =
        let fakeRandom = (fun x -> 0)
        let game = GameGrid.createWithRandomizer fakeRandom
        let expectedGrid = [2;0;0;0] :: List.init 3 (fun _ -> List.init 4 (fun _ -> 0))
        Assert.AreEqual(GameGrid.Running, game.status)
        Assert.AreEqual(0, game.score)
        Assert.AreEqual(expectedGrid, game.grid)

    [<Test>]
    member x.FromArray() =
        let initialArray = [|[|2;4;8;16|]; [|4;8;16;32|]; [|8;16;32;64|]; [|16;8;4;0|]|]
        let expectedGrid = Array.toList (Array.map Array.toList initialArray) 
        let game = GameGrid.fromArray initialArray
        Assert.AreEqual(GameGrid.Running, game.status)
        Assert.AreEqual(0, game.score)
        Assert.AreEqual(expectedGrid, game.grid)

    [<Test>]
    member x.MoveWithoutEffect() =
        let initialGrid = [|[|0;0;0;0|]; [|2;0;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|]
        let game = GameGrid.fromArray initialGrid
        let updatedGame = GameGrid.move game GameGrid.Left
        Assert.AreEqual(game.status, updatedGame.status)
        Assert.AreEqual(game.score, updatedGame.score)
        Assert.AreEqual(game.grid, updatedGame.grid)

    [<Test>]
    member x.MoveWithSimpleMerge() =
        let fakeRandom = (fun x -> 0)
        let grids = [ [|[|0;0;0;0|]; [|2;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|2;0;2;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|2;0;0;2|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;2;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;0;2;2|]; [|0;0;0;0|]; [|0;0;0;0|]|]
        ]
        let expectedGrid = [[2;0;0;0]; [4;0;0;0]; [0;0;0;0]; [0;0;0;0]]
        grids
        |> List.iter (assertGrid GameGrid.Left expectedGrid)

    [<Test>]
    member x.MoveRight() =
        let fakeRandom = (fun x -> 0)
        let grids = [ [|[|0;0;0;0|]; [|2;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|2;0;2;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|2;0;0;2|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;2;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;0;2;2|]; [|0;0;0;0|]; [|0;0;0;0|]|]
        ]
        let expectedGrid = [[2;0;0;0]; [0;0;0;4]; [0;0;0;0]; [0;0;0;0]]
        grids
        |> List.iter (assertGrid GameGrid.Right expectedGrid)

    [<Test>]
    member x.MoveUp() =
        let fakeRandom = (fun x -> 0)
        let grids = [ [|[|0;2;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;2;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]|];
                        [|[|0;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]|]
        ]
        let expectedGrid = [[2;4;0;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]]
        grids
        |> List.iter (assertGrid GameGrid.Up expectedGrid)

    [<Test>]
    member x.MoveDown() =
        let fakeRandom = (fun x -> 0)
        let grids = [ [|[|0;2;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|];
                        [|[|0;2;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]|];
                        [|[|0;2;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]|];
                        [|[|0;0;0;0|]; [|0;2;0;0|]; [|0;0;0;0|]; [|0;2;0;0|]|]
        ]
        let expectedGrid = [[2;0;0;0]; [0;0;0;0]; [0;0;0;0]; [0;4;0;0]]
        grids
        |> List.iter (assertGrid GameGrid.Down expectedGrid)

    [<Test>]
    member x.DetectWin() =
        let initialGrid = [|[|0;0;0;0|]; [|2048;0;0;0|]; [|0;0;0;0|]; [|0;0;0;0|]|]
        let game = GameGrid.fromArray initialGrid
        let updatedGame = GameGrid.move game GameGrid.Left
        Assert.AreEqual(GameGrid.Won, updatedGame.status)

    [<Test>]
    member x.DetectOver() =
        let initialGrid = [|[|2;4;8;16|]; [|4;8;16;32|]; [|8;128;32;64|]; [|16;8;4;0|]|]
        let game = GameGrid.fromArray initialGrid
        let updatedGame = GameGrid.move game GameGrid.Right
        Assert.AreEqual(GameGrid.Over, updatedGame.status)
     