type Report = List<int>

type ReportDirection =
    | Increasing
    | Decreasing
    | Unknown

type UnsafeReason = string

type ReportResult =
    | Safe of ReportDirection
    | Unsafe of UnsafeReason

let LoadReports =
    System.IO.File.ReadAllLines(@".\2024\day2\input_data")
    |> Array.map (fun (line) -> 
        line.Split [| ' ' |]
        |> Array.toList
        |> List.map (fun(s) -> s|> int))
    |> Array.toList

let ProcessItemFromReport value next =
    let direction =
        match value with
        | v when v < next -> Increasing
        | v when v > next -> Decreasing
        | _ -> Unknown
    
    match value with 
    | v when v = next -> Unsafe "equal"
    | v when v > next && v - next > 3 -> Unsafe "decreased too much"
    | v when v > next && v - next = 0 -> Unsafe "decreased none"
    | v when v < next && next - v > 3 -> Unsafe "increased too much"
    | v when v < next && next - v = 0 -> Unsafe "increased none"
    | _ -> Safe direction

let rec ProcessReport items direction =
    match items with
    | [] -> Safe direction
    | [_] -> Safe direction
    | head :: second :: tail ->
        let result = ProcessItemFromReport head second
        printfn "Comparing current item: %A to next item: %A result: %A direction: %A" head second result direction
        match result with 
        | Unsafe reason -> Unsafe reason
        | Safe d -> 
            match direction with
            | Unknown -> 
                ProcessReport (second :: tail) d
            | Increasing -> 
                if d = Increasing then 
                    ProcessReport (second :: tail) d
                else
                    Unsafe "changed direction"
            | Decreasing -> 
                if d = Decreasing then 
                    ProcessReport (second :: tail) d
                else
                    Unsafe  "changed direction"
        
let ProcessReports reports = 
    reports
    |> List.map (fun(r)-> 
        printf "Maping %A" r
        ProcessReport r Unknown)

let CountSafeReports reports =
    reports
    |> List.filter (fun(r) -> r = Safe Increasing || r = Safe Decreasing )
    |> List.length

let safe = LoadReports |> ProcessReports |> CountSafeReports
printf "Safe Reports: %A" safe



