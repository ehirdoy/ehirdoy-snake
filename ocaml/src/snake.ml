open Common

type point = int * int
type apple = point

type t = point list

let make ~len = range 1 len |> List.map ~f:(fun n -> (n, 1))
  
let eat t ~apple = apple :: t

let rec mem t ~point =
  let (x, y) = point in
  match t with
  | [] -> false
  | (x', y') :: t' ->
    if x = x' && y = y' then
      true
    else
      mem t' ~point

let move t ~dir ~dim =
  let rec pop_end = function
    | []      -> []
    | [x]     -> []
    | x :: xs -> x :: pop_end xs
  in
  let (x, y) = List.hd t in
  let new_head =
    match dir with
    | `Up -> (x, y - 1)
    | `Down -> (x, y + 1)
    | `Right -> (x + 1, y)
    | `Left -> (x - 1, y)
  in
  let t = pop_end t in

  (* monad *)
  let open Option.Monad_infix in

  (* checks *)
  let in_bounds (x, y as new_head) =
    let width, height = dim in
    if (x >= 1 && x < width - 2) && (y >= 1 && y < height - 2) then
      Some new_head
    else
      None
  in
  let in_space x =
    match mem t ~point:x with
    | true -> None
    | false -> Some x
  in

  new_head
  |> Option.return
  >>= in_bounds
  >>= in_space
  >|= fun h -> h :: t
