type t = {
  snake: Snake.t,
  score: int,
  dir: Direction.t,
  dir_mutex: [ | `Released | `Acquired],
  apple: (int, int),
  is_alive: bool,
  dim: (int, int),
  timeoutId: option(Js.Global.intervalId),
  running: bool
};

let can_eat_apple = (snake, apple, dir) =>
  switch snake {
  | [] => false
  | [(sx, sy), ..._] =>
    let (ax, ay) = apple;
    let apple_pos =
      if (sy - 1 == ay && sx == ax) {
        Some(`Up)
      } else if (sy + 1 == ay && sx == ax) {
        Some(`Down)
      } else if (sx + 1 == ax && sy == ay) {
        Some(`Right)
      } else if (sx - 1 == ax && sy == ay) {
        Some(`Left)
      } else {
        None
      };
    switch (dir, apple_pos) {
    | (x, Some(y)) when x == y => true
    | _ => false
    }
  };

let next_state = (state) => {
  let ate_apple = ref(false);
  let snake =
    if (state.is_alive) {
      let s = state.snake;
      let a = state.apple;
      let d = state.dir;
      if (can_eat_apple(s, a, d)) {
        ate_apple := true;
        Some(Snake.eat(s, ~apple=a))
      } else {
        Snake.move(state.snake, ~dir=state.dir, ~dim=state.dim)
      }
    } else {
      None
    };
  switch snake {
  | Some(snake) =>
    if (ate_apple^) {
      let (w, h) = state.dim;
      /* yeah, apples can spawn under a snake */
      let ax = Random.int(w - 1);
      let ay = Random.int(h - 1);
      {...state, snake, apple: (ax, ay), score: state.score + 1}
    } else {
      {...state, snake}
    }
  | None => {...state, is_alive: false, running: false}
  }
};

let initial_state = {
  snake: Snake.make(~len=1, ~pos=(0, 0)),
  score: 0,
  dir: `Down,
  dir_mutex: `Released,
  apple: (1, 10),
  is_alive: true,
  dim: (25, 25),
  timeoutId: None,
  running: false
};

let change_direction = (state, dir) =>
  switch state.dir_mutex {
  | `Acquired => state
  | `Released =>
    dir == Direction.opp_dir(state.dir) ? state : {...state, dir, dir_mutex: `Acquired}
  };
