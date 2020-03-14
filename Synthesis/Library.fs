module Synthesis

let abelar a =
    let b = 
        match (a % 12, a > 12, a < 3097) with 
        | (0, true, true) -> true
        | _ -> false
    b
    
let area b h =
    let a =
        match (b >= 0.0, h >= 0.0) with
        | (true, true) -> 0.5 * b * h
        | _ -> failwith "negative number"    
    a
    
let zollo a =
    let b = 
        match (a >= 0) with
        | true -> a * 2
        | false -> -a
    b

let min a b =
    let c = min a b
    c

let max a b =
    let c = max a b
    c

let ofTime h m s =
    s + (m * 60) + (h * 60 * 60)
    

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax (a, b, c, d) =
    (min (min a b) (min c d), max (max a b) (max c d))

let isLeap y =
    let a =
        match (y > 1581, y % 4 = 0, y % 400 = 0, y % 100 = 0) with
        | (true, true, true, true) -> true
        | (true, true, _, false) -> true
        | (false, _, _, _)-> failwith "Unsupported input"
        | _ -> false
    a

let month a =
    let b = 
        match a with
        |1 -> ("January", 31)
        |2 -> ("February", 28)
        |3 -> ("March", 31)
        |4 -> ("April", 30)
        |5 -> ("May", 31)
        |6 -> ("June", 30)
        |7 -> ("July", 31)
        |8 -> ("August", 31)
        |9 -> ("September", 30)
        |10 -> ("October", 31)
        |11 -> ("November", 30)
        |12 -> ("December", 31)
        |_ -> failwith "Invalid month"
    b

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"