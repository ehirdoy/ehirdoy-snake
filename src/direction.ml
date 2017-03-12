type t = [`Up | `Down | `Left | `Right]

let opp_dir = function
  | `Up -> `Down
  | `Down -> `Up
  | `Left -> `Right
  | `Right -> `Left
