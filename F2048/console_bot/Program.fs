// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open F2048.GameGrid
open F2048.Bot
open System

let oneGame () =
    let game = F2048.GameGrid.create()
    let rec play iter engine g =
            let newGame = F2048.GameGrid.move g (engine g)
      //      printfn "%s" (F2048.GameGrid.toString newGame) 
            match newGame.status with
            | Running -> play (iter+1) engine newGame
            | Over -> 
                    printfn "Game Over score:%d moves:%d" newGame.score iter
                    Over
            | Won ->
                    printfn "Game Won score:%d moves:%d" newGame.score iter
                    Won
    // printfn "%s" (F2048.GameGrid.toString game) 
    play 0 F2048.Bot.bit_clever game

let stats () =
    let won, lost = [1..100]
                    |> List.map (fun _ -> oneGame())
                    |> List.partition (fun x -> match x with
                                                |Won -> true
                                                | _ -> false)
    printfn "100 games played: %d won %d lost" (List.length won) (List.length lost)

[<EntryPoint>]
let main argv =

    stats ()
    printfn "press [enter] to exit"
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code

