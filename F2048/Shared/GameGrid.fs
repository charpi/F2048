namespace F2048

open System

module GameGrid =
    
    type Move = Left | Right | Up | Down
    type Row = int list
    type Grid = Row list
    type ScoredGrid = (int*int) list list
    type Score = int
    type GameStatus = Over | Won | Running
    type Game = {score :Score ; grid :Grid ; status : GameStatus}
   
    let dim = 4
    let emptyGrid :Grid =
        List.init dim (fun _ -> List.init dim (fun _ -> 0))

    let newTile grid =
        let rnd = new System.Random()
        let randomValue () =
            let values = [|2;2;2;2;2;2;2;2;2;4|]
            values.[rnd.Next(0,9)]
        let rec choose (innerRnd :System.Random) (innerArray :int array []) =
            let row = innerArray.[innerRnd.Next(0, dim)]
            let index = innerRnd.Next(0,dim)
            match row.[index] with
            |0 ->
                row.[index] <- randomValue ()
                innerArray
            |_ ->
                choose innerRnd innerArray
        grid
        |> List.map List.toArray
        |> List.toArray
        |> choose rnd
        |> Array.map Array.toList
        |> Array.toList

    let create () :Game =
        {score=0; grid = (newTile emptyGrid) ; status = Running}
    
    let merge_left (input :Row) :(int* int) list = 
        let pad length inp =
             List.append inp (List.init (length - inp.Length) (fun _ -> (0,0)))
        let rec mergeValues values =
            match values with
            | x :: y :: xs ->
                if x = y then 
                    (x*2, x*2) :: mergeValues xs 
                else
                    (x, 0) :: mergeValues (y :: xs)
            | x :: xs ->
                (x, 0) :: mergeValues xs
            | [] ->
                []
        input
        |> List.filter (fun x -> not (x = 0))
        |> mergeValues
        |> pad (input.Length)

    
    let rec moveGrid (m :Move) (grid :Grid) :ScoredGrid =
        let rec transpose (nullValue :'T) (gr :'T list list) :'T list list = 
                match gr with
                | a :: _ -> match a with
                            | z :: zs ->
                                let heads = gr |> List.map (fun l -> match l with 
                                                                     | x :: xs -> x
                                                                     | _ -> nullValue )
                                let tails = List.map (fun l -> match l with 
                                                                  | x :: xs -> xs
                                                                  | _ -> []
                                                     ) gr
                                heads :: transpose nullValue tails
                            | _ -> []
                | _ -> gr
        match m with
            | Left -> 
                grid
                |> List.map (fun x -> merge_left x)
            | Right -> 
                grid 
                |> List.map (fun x -> List.rev x)
                |> moveGrid Left
                |> List.map (fun x -> List.rev x)
            | Up -> 
                grid
                |> transpose 0
                |> moveGrid Left
                |> transpose (0,0)
            | Down ->
                grid
                |> transpose 0
                |> moveGrid Right 
                |> transpose (0,0)

    let extractPoint (sg :ScoredGrid) :(Grid * int) =
        let g,p = sg 
                    |> List.map (fun p -> List.unzip p)
                    |> List.fold (fun (lines, pts) (l,y) ->
                                    (l :: lines, pts + (List.sum y)))
                                 ([], 0)
        (List.rev g, p)

    let isSameGrid a b =
        List.zip a b
        |> List.forall (fun (x,y) -> x = y)

    let statusFromGrid g =
        let values = (List.collect (fun x ->x) g)
        if List.exists (fun x -> x = 2048) values then
            Won
        else 
            if List.exists (fun x -> x = 0) values then
                Running
            else
                let notFinished = [Left;Right;Up;Down]
                                    |> List.map (fun m -> extractPoint (moveGrid m g))
                                    |> List.exists (fun (_,p) -> p <> 0)
                if notFinished then Running else Over

    let move (g: Game) (m :Move) =
        let gr = g.grid
        let sc = g.score
        let mergedGrid, points = extractPoint (moveGrid m gr)
        let newGrid = if not (isSameGrid mergedGrid gr) then (newTile mergedGrid) else mergedGrid
        {status = (statusFromGrid newGrid); score = sc + points; grid = newGrid}

    let equal a b =
            a.score = b.score && a.grid = b.grid

    let toString (g: Game) =
        g.grid
        |> List.map (fun row -> row |> List.fold (fun acc v -> sprintf "%s %5d" acc v) "")
        |> List.fold (fun acc row -> sprintf "%s\n%s" acc row) ""