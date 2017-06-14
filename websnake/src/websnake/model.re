open Common;

open Printf;

type t = {
  snake: Snake.t,
  dir: Direction.t,
  apple: (int, int),
  is_alive: bool,
  dim: (int, int),
  timeoutId: option int
};

let can_eat_apple snake apple dir =>
  switch snake {
  | [] => false
  | [(sx, sy), ..._] =>
    let (ax, ay) = apple;
    let apple_pos =
      if (sy - 1 == ay && sx == ax) {
        Some `Up
      } else if (sy + 1 == ay && sx == ax) {
        Some `Down
      } else if (
        sx + 1 == ax && sy == ay
      ) {
        Some `Right
      } else if (
        sx - 1 == ax && sy == ay
      ) {
        Some `Left
      } else {
        None
      };
    switch (dir, apple_pos) {
    | (x, Some y) when x == y => true
    | _ => false
    }
  };

let next_state state => {
  let ate_apple = ref false;
  let snake =
    if state.is_alive {
      let s = state.snake;
      let a = state.apple;
      let d = state.dir;
      if (can_eat_apple s a d) {
        ate_apple := true;
        Some (Snake.eat s apple::a)
      } else {
        Snake.move state.snake dir::state.dir dim::state.dim
      }
    } else {
      None
    };
  switch snake {
  | Some snake =>
    if !ate_apple {
      let (w, h) = state.dim;
      let ax = Random.int (w - 3) + 1;
      let ay = Random.int (h - 3) + 1;
      {...state, snake, apple: (ax, ay)}
    } else {
      {...state, snake}
    }
  | None => {...state, is_alive: false}
  }
};

let initial_state = {
  snake: Snake.make len::10,
  dir: `Down,
  apple: (0, 0),
  is_alive: true,
  dim: (0, 0),
  timeoutId: None
};

let change_direction state dir => dir == Direction.opp_dir state.dir ? state : {...state, dir};
