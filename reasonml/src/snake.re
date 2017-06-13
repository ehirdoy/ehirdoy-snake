open Common;

type point = (int, int);
type apple = point;

type t = list point;

let make ::len => range 1 len |> List.map f::(fun n => (n, 1));
  
let eat t ::apple => [apple, ...t];

let rec mem t ::point => {
  let (x, y) = point;
  switch t {
  | [] => false
  | [(x', y'), ...t'] =>
    if (x == x' && y == y') {
      true
    } else {
      mem t' point::point
    };
  };
};

let move t ::dir ::dim => {
  let rec pop_end snake => {
    switch snake {
    | []      => []
    | [x]     => []
    | [x, ...xs] => [x, ...pop_end xs]
    };
  };
  let (x, y) = List.hd t;
  let new_head =
    switch dir {
    | `Up => (x, y - 1)
    | `Down => (x, y + 1)
    | `Right => (x + 1, y)
    | `Left => (x - 1, y)
    };
  let t = pop_end t;

  let open Option.Monad_infix;

  /* checks */
  let in_bounds ((x, y) as new_head) => {
    let (width, height) = dim;
    if ((x >= 1 && x < width - 2) && (y >= 1 && y < height - 2)) {
      Some new_head
    } else {
      None
    }
  };
  let in_space x => {
    switch (mem t point::x) {
    | true => None
    | false => Some x
    };
  };

  new_head
  |> Option.return
  >>= in_bounds
  >>= in_space
  >|= fun h => [h, ...t]
};
