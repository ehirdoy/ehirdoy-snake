open Notty

let blank_space _      = I.void 1 1
let border_piece hex c = I.uchar A.(fg c) hex 1 1
let snake_piece c      = I.uchar A.(fg c) 0x1f40d 1 1
let dead_piece         = I.uchar A.(fg lightred) 0x26b0 1 1
let live_piece         = snake_piece A.lightgreen
let top_edge           = border_piece 0x2594
let bottom_edge        = border_piece 0x2581
let left_edge          = border_piece 0x258f
let right_edge         = border_piece 0x2595
let tl_corner          = border_piece 0x25e4
let tr_corner          = border_piece 0x25e5
let bl_corner          = border_piece 0x25e3
let br_corner          = border_piece 0x25e2
let apple_piece        = I.uchar A.(fg lightred) 0x1f34e 1 1

let background (x, y) ~dim =
  let (width, height) = dim in
  let x', y' = width - 2, height - 2 in
  let piece =
    match x, y with
    | 0, 0                       -> tl_corner
    | 0, y when y = y'           -> bl_corner
    | x, 0 when x = x'           -> tr_corner
    | x, y when x = x' && y = y' -> br_corner
    | 0, _                       -> left_edge
    | _, 0                       -> top_edge
    | x, _ when x = x'           -> right_edge
    | _, y when y = y'           -> bottom_edge
    | _, _                       -> blank_space
  in
  piece A.lightwhite

let snake_color is_alive =
  match is_alive with
  | true -> live_piece
  | false -> dead_piece

let apple = apple_piece
