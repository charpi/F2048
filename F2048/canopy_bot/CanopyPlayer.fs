namespace canopy_bot

open System
open canopy
open canopy.core

open F2048.GameGrid
open F2048.Bot

module CanopyPlayer =
    let baseURL = "http://gabrielecirulli.github.io/2048/"

    type Score = string * int * string

    type Position = int * int
    type Value = int
    type Tile = Position * Value
    type Grid = int [][]
    type GameStatus = Over | Won | Running

 //   type Move = Left | Right | Up | Down
   
    let statusToString = function 
                        | Over -> "Lost"
                        | Won -> "Won"
                        | Running -> "Running"

    let tileToGrid tiles :Grid =
        let defaultGrid = Array.init 4 (fun _ -> Array.init 4 (fun _ -> 0)) in
        tiles
        |> List.fold (fun acc ((x :int, y :int ), v :Value) -> acc.[y-1].SetValue(v,x-1)
                                                               acc ) defaultGrid
        
    let gridToGame (grid :Grid) :F2048.GameGrid.Game =
        {score = 0; grid = Array.toList (Array.map (fun x -> Array.toList x) grid)}

    let addScore (game :F2048.GameGrid.Game) =
         let sc = (element ".score-container").Text
         { game with score = int (sc.Split([|'+'|]).[0])}

    let newTile v x y :Tile =
        ((x , y), v)

    let classToTile (html :String) =
        let pattern = html.Split [|' '|]
        let tileValue = pattern.[1]
        let tilePosition = pattern.[2]
        let value = (tileValue.Split [|'-'|]).[1]
        let position= (tilePosition.Split [|'-'|])
        let x = position.[2]
        let y = position.[3]
        newTile (int value) (int x) (int y)

    let htmlToTile html =
        html
        |> List.map (fun (t:OpenQA.Selenium.IWebElement) ->
                        classToTile (t.GetAttribute("class"))
                    )

    let moveToCanopy (m :F2048.GameGrid.Move) = match m with
                                                | Left -> left
                                                | Right -> right
                                                | Up -> up
                                                | Down -> down

    let rec doPlay engine iter :Score =
      let gameStatus = match (someElement ".game-over") with
                        |Some (x) -> Over
                        | None -> match (someElement ".game-won") with
                                    |Some (x) -> Won
                                    | None -> Running
      match gameStatus with
        | Running ->
            let steps = element ".tile-container"
                             |> elementsWithin ".tile"
                             |> htmlToTile
                             |> tileToGrid
                             |> gridToGame
                             |> addScore
                             |> engine
                             |> moveToCanopy
                             |> press
                             |> ignore in
             doPlay engine (iter+1)
        | x ->
              sleep(1)
              let score = (element ".score-container").Text in
              quit()
              (statusToString x, iter, score)

    let play (engine: F2048.Bot.fct) :Score =
        start firefox
        url baseURL
        doPlay engine 0




