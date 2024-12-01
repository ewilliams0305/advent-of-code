let public ReadLines =
    System.IO.File.ReadAllLines(@".\2024\day1\input_data")
    |> Array.toList
    
let TupledItem (line:string) =
    let split = line.Split([| "   " |], System.StringSplitOptions.None)
    (split[0], split[1])
    
let CreateTupleList (lines:List<string>) =
    lines
    |> List.map (fun (line) -> TupledItem(line))
    |> List.map (fun (a, b) -> a |> int, b |> int)
    
let CreateLeftFromTuple (items: List<(int * int)>) =
    items
    |> List.map(fun(a, b) -> a)
    |> List.sort
    
let CreateRightFromTuple (items: List<(int * int)>) =
    items
    |> List.map(fun(a, b) -> b)
    |> List.sort

let CaluculateSimilarity (items: List<(int * int)>) =
    // let items = ReadLines |> CreateTupleList
  
    let left = items |> CreateLeftFromTuple    
    let right = items |> CreateRightFromTuple

    let similarity =
        left 
        |> List.map (fun left -> 
            let count = 
                right 
                |> List.filter (fun right -> right = left)
                |> List.length
            
            left * count
        )
        |> List.sum

    similarity
        
let total = CaluculateSimilarity (ReadLines |> CreateTupleList)
printf "TOTAL: %A" total