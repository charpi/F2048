// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open F2048.GameGrid
open F2048.Bot
open System
open System.Diagnostics

let oneGame () =
    let game = F2048.GameGrid.create()
    let rec play iter engine g same =
            if same = 10 then
                printfn "%s" (F2048.GameGrid.toString g)
                printfn "Sound like a loop somewhere 10 moves without changes"
                (Over, g.score, iter)
            else
                let newGame = F2048.GameGrid.move g (engine g)
                //printfn "%s" (F2048.GameGrid.toString newGame) 
                match newGame.status with
                | Running -> play (iter+1) engine newGame (if (F2048.GameGrid.equal newGame g) then (same+1) else 0)
                | Over -> 
                        (Over,newGame.score,iter)
                | Won ->
                        (Won,newGame.score,iter)
        

     // printfn "%s" (F2048.GameGrid.toString game) 
    let stopWatch = Stopwatch.StartNew()
    let st, sc, it = play 0 F2048.Bot.bit_clever game 0
    stopWatch.Stop()
    let d = stopWatch.Elapsed.TotalMilliseconds
    printfn "Game %A score:%d moves:%d duration:%f" st  sc  it  d
    (st, sc, it, stopWatch.Elapsed.TotalMilliseconds)

let stats () =
    let won, lost = [1..100]
                    |> List.map (fun _ -> oneGame())
                    |> List.partition (fun (x,_,_,_) -> match x with
                                                        |Won -> true
                                                        | _ -> false)
    printfn "100 games played: %d won %d lost" (List.length won) (List.length lost)

[<EntryPoint>]
let main argv =

    stats ()
    printfn "press [enter] to exit"
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code

