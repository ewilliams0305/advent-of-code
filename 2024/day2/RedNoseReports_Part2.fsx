type Report = List<int>

type ReportDirection =
    | Increasing
    | Decreasing
    | Unknown

type UnsafeReason = string

type ReportResult =
    | Safe of ReportDirection
    | Unsafe of UnsafeReason
 
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
        match result with 
        | Unsafe reason -> Unsafe reason
        | Safe d -> 
            match direction with
            | Unknown -> ProcessReport (second :: tail) d
            | Increasing when d = Increasing -> ProcessReport (second :: tail) d
            | Decreasing when d = Decreasing -> ProcessReport (second :: tail) d
            | _ -> Unsafe "changed direction"

let removeOneAtATime (items: 'a list) =
    items
    |> List.mapi (fun index _ -> 
        items |> List.mapi (fun i x -> if i <> index then Some x else None) 
              |> List.choose id)

let ProcessSingleReport report =
    match ProcessReport report Unknown with
    | Safe _ as result -> result
    | Unsafe _ ->
        let subReports = removeOneAtATime report
        subReports
        |> List.tryFind (fun subReport -> 
            match ProcessReport subReport Unknown with
            | Safe _ -> true
            | Unsafe _ -> false)
        |> function
           | Some _ -> Safe Unknown 
           | None -> Unsafe "two or more unsafe items"

let ProcessReports reports =
    reports
    |> List.map ProcessSingleReport

let CountSafeReports reports =
    reports
    |> List.filter (fun(r) ->
        match r with
        | Safe _ -> true
        | Unsafe _ -> false)
    |> List.length

let testReports = 
    [
        [7; 6; 4; 2; 1]
        [1; 2; 7; 8; 9]
        [9; 7; 6; 2; 1]
        [1; 3; 2; 4; 5]
        [8; 6; 4; 4; 1]
        [1; 3; 6; 7; 9]
    ]

let results = ProcessReports LoadReports

printfn "Results: %A" results

let safe = results |> CountSafeReports
let total = results |> List.length
printf "Safe Reports: %A of %A" safe total
