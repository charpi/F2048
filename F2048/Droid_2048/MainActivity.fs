namespace Droid_2048

open System

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open System.Data
open Mono.Data.Sqlite

open F2048.GameGrid
open F2048.Bot

[<Activity (Label = "Droid_2048", MainLauncher = true)>]
type MainActivity () =
    inherit Activity ()

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
        let commands = [ "CREATE TABLE [Game] (Key ntext PRIMARY KEY, Value ntext);"; "INSERT INTO [Game] ([Key], [Value]) VALUES ('best', '0')"]
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

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)

        // Set our view from the "main" layout resource
        this.SetContentView (Resource_Layout.Main)

        // Get our button from the layout resource, and attach an event to it
        let button = this.FindViewById<Button>(Resource_Id.reset)
        let score = this.FindViewById<TextView>(Resource_Id.score)
        let board = this.FindViewById<TextView>(Resource_Id.board)
        let best = this.FindViewById<TextView>(Resource_Id.best)
        let test = this.FindViewById<TextView>(Resource_Id.test)

        let refreshGame () = 
            score.Text <- sprintf "%d" (game.score)
            board.Text <- F2048.GameGrid.toString game
            best.Text <- readBest ()

        button.Click.Add (fun args -> 
            game <- F2048.GameGrid.create
            if (int best.Text) < (int score.Text) then writeBest score.Text else ()
            refreshGame ()
        )

        test.Click.Add (fun args ->
                test.Text <- "Running"
                let refresh = Action refreshGame
                let a = async { let rec tmp = function 
                                                | 0 -> ()
                                                | n -> 
                                                    let nextMove = F2048.Bot.bit_clever game
                                                    game <- F2048.GameGrid.move game nextMove
                                                    ignore(board.Post(refresh))
                                                    tmp (n-1)
                                tmp 500
                                test.Text <- "Bot done"
                                ignore(board.Post(refresh))
                            
                        }
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
                    refreshGame ()
                | _ ->
                    () // skip
        )

        refreshGame ()