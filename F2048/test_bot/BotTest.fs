namespace F2048
open System
open NUnit.Framework

open GameGrid
open Bot

[<TestFixture>]
type BotTest() = 

    [<Test>]
    member x.LastMoveOfTheGame() =
        let a = [|[|2;4;8;16|]; [|4;8;16;32|]; [|8;16;32;64|]; [|16;8;4;0|]|]
        let game = GameGrid.fromArray a
        let m = Bot.bit_clever game
        Assert.AreEqual(m, F2048.GameGrid.Down)
