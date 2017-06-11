module List = ListLabels
let rec range a b =
  if a > b
    then []
    else a :: range (succ a) b

