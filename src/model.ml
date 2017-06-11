open Common
open Printf

type t = {
  snake    : Snake.t;
  dir      : Direction.t;
  apple    : int * int;
  is_alive : bool;
  dim      : int * int;
}

let can_eat_apple snake apple dir =
  let head =
    match snake with
    | [] -> None
    | x::xs -> Some x
  in
  let check =
    Option.map head ~f:(fun (x,y) ->
      let (ax, ay) = apple in
      match dir with
      | `Up when y-1=ay && x=ax -> true
      | `Down when y+1=ay && x=ax -> true
      | `Right when x+1=ax && y=ay -> true
      | `Left when x-1=ax && y=ay -> true
      | _ -> false
    )
  in
  match check with
  | Some x -> x
  | None -> false

let next_state state =
  let ate_apple = ref false in
  let snake =
    if state.is_alive then
      let s = state.snake in
      let a = state.apple in
      let d = state.dir in
      if can_eat_apple s a d then begin
        ate_apple := true;
        Some (Snake.eat s ~apple:a)
      end else
        Snake.move state.snake ~dir:state.dir ~dim:state.dim
    else
      None
  in
  match snake with
  | Some snake ->
      if !ate_apple then
        let w, h = state.dim in
        let ax = (Random.int (w - 2)) + 1 in
        let ay = (Random.int (h - 2)) + 1 in
        { state with snake = snake; apple = (ax, ay) }
      else
        { state with snake = snake }
  | None -> { state with is_alive = false }

let initial_state = {
  snake    = Snake.make ~len:10;
  dir      = `Down;
  apple    = (0, 0);
  is_alive = true;
  dim      = (0, 0)
}

let change_direction state dir =
  match dir = Direction.opp_dir state.dir with
  | true  -> state
  | false -> { state with dir = dir }

