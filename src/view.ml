open Notty

let dot c = I.uchar A.(fg c) 0x1F40D 1 1
let dead_dot = dot A.lightred
let live_dot = dot A.lightgreen
let background (x, y) ~dim =
  let (width, height) = dim in
  if x = 0 || x = width - 2 || y = 0 || y = height - 2 then
    dot A.lightblack
  else
    I.void 1 1

let snake_color is_alive =
  match is_alive with
  | true -> live_dot
  | false -> dead_dot
