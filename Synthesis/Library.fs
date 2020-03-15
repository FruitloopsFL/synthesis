module Synthesis

let abelar a =
    match (a % 12, a > 12, a < 3097) with 
    | (0, true, true) -> true
    | _ -> false

    
let area b h =
    match (b >= 0.0, h >= 0.0) with
    | (true, true) -> 0.5 * b * h
    | _ -> failwith "negative number"    

    
let zollo a =
    match (a >= 0) with
    | true -> a * 2
    | false -> -a
    

let min a b =
    match a > b with
    | true -> b
    | false -> a


let max a b =
    match a > b with
    | true -> a
    | false -> b


let ofTime h m s =
    s + (m * 60) + (h * 3600)
    

let toTime s =
    match s < 0 with
    | true -> (0, 0, 0)
    | false -> (s/3600, (s%3600)/60, (s%3600)%60)
    

let digits x =
    let rec counter x v =
        match (x / 10 >= 1 || x / 10 <= -1) with
        | false -> v
        | true -> counter (x / 10) (v + 1)
    counter x 1

let minmax (a, b, c, d) =
    (min (min a b) (min c d), max (max a b) (max c d))

let isLeap y =
    match (y > 1581, y % 4 = 0, y % 400 = 0, y % 100 = 0) with
    | (true, true, true, true) -> true
    | (true, true, _, false) -> true
    | (false, _, _, _)-> failwith "Unsupported input"
    | _ -> false
    

let month a =
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
   

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay d y =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"