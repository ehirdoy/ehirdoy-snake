open Common;

type point = (int, int);

type apple = point;

type t = list point;

let make ::len ::pos => {
  let (x,y) = pos;
  let len = len - 1;
  range x (x+len) |> List.map f::(fun n => (n, y));
};

let eat t ::apple => [apple, ...t];

let rec mem t ::point => {
  let (x, y) = point;
  switch t {
  | [] => false
  | [(x', y'), ...t'] =>
    if (x == x' && y == y') {
      true
    } else {
      mem t' ::point
    }
  }
};

let move t ::dir ::dim => {
  let rec pop_end snake =>
    switch snake {
    | [] => []
    | [x] => []
    | [x, ...xs] => [x, ...pop_end xs]
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
  open Option.Monad_infix;
  /* checks */
  let in_bounds ((x, y) as new_head) => {
    let (width, height) = dim;
    if ((x >= 0 && x < width-1) && y >= 0 && y < height-1) {
      Some new_head
    } else {
      None
    }
  };
  let in_space x => mem t point::x ? None : Some x;
  new_head |> Option.return >>= in_bounds >>= in_space >|= (fun h => [h, ...t])
};
