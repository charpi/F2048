namespace Droid_2048

open System
open System.Data

open Mono.Data.Sqlite

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget

open F2048.GameGrid
open F2048.Bot

[<Activity (Label = "F2048", MainLauncher = true)>]
type MainActivity () =
    inherit Activity ()

    let mutable botRunning = false
    let mutable game = F2048.GameGrid.create
    let mutable originX = 0.0
    let mutable originY = 0.0

    let databasePath = 
        System.IO.Path.Combine ((Environment.GetFolderPath Environment.SpecialFolder.Personal),"droid_2048.db")
    
    let firstBoot =
        if (System.IO.File.Exists databasePath) then
            false
        else
            Mono.Data.Sqlite.SqliteConnection.CreateFile databasePath
            true

    let cnx = 
        let tmp = new Mono.Data.Sqlite.SqliteConnection("Data Source=" + databasePath)
        tmp.Open()
        tmp

    let initdb =
        let g = F2048.GameGrid.create
        let commands = [ "CREATE TABLE [Game] (Key ntext PRIMARY KEY, Value ntext);" ; "INSERT INTO [Game] ([Key], [Value]) VALUES ('best', '0') , ('game', '" + (F2048.GameGrid.toStringWithScore g) + "')"
                        ]
        if (firstBoot = false) then 
            ()
        else
            commands
            |> List.iter (fun cmd ->
                            let ctx = cnx.CreateCommand()
                            ctx.CommandText <- cmd
                            ignore (ctx.ExecuteNonQuery())
                            )

    let readBest () = 
        let cmd = "SELECT [Value] from [Game] where [Key] = 'best'"
        let ctx = cnx.CreateCommand()
        ctx.CommandText <- cmd
        let reader = ctx.ExecuteReader()
        ignore (reader.Read())
        reader.GetString(0)

    let writeBest value =
        let cmd = "UPDATE [Game] set Value = '" + value + "' WHERE Key = 'best'"
        let ctx = cnx.CreateCommand()
        ctx.CommandText <- cmd
        ignore(ctx.ExecuteNonQuery())

    let writeGame (g :Game) =
        let cmd = "UPDATE [Game] set Value = '" + (F2048.GameGrid.toStringWithScore g) + "' WHERE Key = 'game'"
        let ctx = cnx.CreateCommand()
        ctx.CommandText <- cmd
        ignore(ctx.ExecuteNonQuery())

    let readGame () =
        let cmd = "SELECT [Value] from [Game] where [Key] = 'game'"
        let ctx = cnx.CreateCommand()
        ctx.CommandText <- cmd
        let reader = ctx.ExecuteReader()
        ignore (reader.Read())
        F2048.GameGrid.fromStringWithScore (reader.GetString(0))

    let play (dx :float) (dy :float) =
        let absX = abs dx
        let absY = abs dy
        if (absX + absY) > 50. then
                let move = if absX > absY then 
                            (if (dx > 0.0) then F2048.GameGrid.Left else F2048.GameGrid.Right)
                            else
                            (if (dy > 0.0) then F2048.GameGrid.Up else F2048.GameGrid.Down)
                F2048.GameGrid.move game move
        else
            game

    let refreshGame (score :TextView) (board :TextView) = 
            score.Text <- sprintf "Score\n%d" (game.score)
            board.Text <- F2048.GameGrid.toString game

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)
        this.SetContentView (Resource_Layout.Main)
        let button = this.FindViewById<Button>(Resource_Id.reset)
        let score = this.FindViewById<TextView>(Resource_Id.score)
        let board = this.FindViewById<TextView>(Resource_Id.board)
        let best = this.FindViewById<TextView>(Resource_Id.best)
        let test = this.FindViewById<TextView>(Resource_Id.test)
        test .Text <- "Bot"

        let oldBest = int (readBest())

        button.Click.Add (fun args -> 
            if oldBest < game.score then writeBest (string game.score) else ()
            game <- F2048.GameGrid.create
            refreshGame score board
            best.Text <- sprintf "Best\n%s" (readBest ())
        )

        test.Click.Add (fun args ->
                test.Text <- " Bot Running"
                test.Enabled <- false
                let refresh = Action (fun() -> refreshGame score board)
                let endBot = Action (fun () ->
                                    test.Text <- "Bot"
                                    test.Enabled <- true
                                    refreshGame score board 
                                    botRunning <- false
                                    )
                let a = async { let rec tmp = function 
                                                | 0 -> ()
                                                | n -> 
                                                    let nextMove = F2048.Bot.bit_clever game
                                                    game <- F2048.GameGrid.move game nextMove
                                                    ignore(board.Post(refresh))
                                                    tmp (n-1)
                                tmp 100
                                ignore(board.Post(endBot))
                            
                        }
                botRunning <- true
                Async.Start a
        )


        board.Touch.Add (fun (args :View.TouchEventArgs) ->
             match args.Event.Action with
                | MotionEventActions.Down ->
                    originX <- (float) args.Event.RawX
                    originY <- (float) args.Event.RawY
                | MotionEventActions.Up ->
                    let deltaX = originX - (float) args.Event.RawX
                    let deltaY = originY - (float) args.Event.RawY
                    originX <- 0.0
                    originY <- 0.0
                    game <- (play deltaX deltaY)
                    refreshGame score board
                | _ ->
                    () // skip
        )

    override this.OnPause() =
        base.OnPause()
        if botRunning then () else (writeGame game)

    override this.OnResume() =
        base.OnResume()
        let score = this.FindViewById<TextView>(Resource_Id.score)
        let board = this.FindViewById<TextView>(Resource_Id.board)
        if botRunning  then
            ()
        else 
            game <- readGame()
            refreshGame score board