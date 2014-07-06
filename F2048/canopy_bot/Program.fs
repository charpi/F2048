// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//namespace canopy_bot
 
open System
open canopy_bot.CanopyPlayer
open F2048.Bot

[<EntryPoint>]
let main argv = 
    let status, iter, score = canopy_bot.CanopyPlayer.play F2048.Bot.bit_clever
    printfn "Bot %s in %d moves with a score of %d" status iter score
    printfn "press [enter] to exit"
    System.Console.ReadLine() |> ignore
    0
