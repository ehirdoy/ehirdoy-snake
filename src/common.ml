module List = ListLabels
let rec ( --> ) a b = if a > b then [] else a :: succ a --> b

