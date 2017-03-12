open Common
open Printf

type t = {
  snake    : Snake.t;
  dir      : Direction.t;
  apple    : int * int;
  is_alive : bool;
  dim      : int * int;
}

let next_state state =
  let snake =
    if state.is_alive then
      Snake.move state.snake ~dir:state.dir ~dim:state.dim
    else
      None
  in
  match snake with
  | Some snake -> { state with snake = snake }
  | None       -> { state with is_alive = false }

let initial_state = {
  snake    = Snake.make ~len:10;
  dir      = `Down;
  apple    = (0, 0);
  is_alive = true;
  dim      = (0, 0)
}

let change_dir state dir =
  match dir = Direction.opp_dir state.dir with
  | true  -> state
  | false -> { state with dir = dir }

