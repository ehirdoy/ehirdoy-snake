module List = ListLabels;

let rec range = (a, b) =>
  if (a > b) {
    []
  } else {
    [a, ...range(succ(a), b)]
  };

module Option = {
  let map = (t, ~f) =>
    switch t {
    | Some(x) => Some(f(x))
    | None => None
    };
  let bind = (t, ~f) =>
    switch t {
    | Some(x) => f(x)
    | None => None
    };
  let return = (x) => Some(x);
  module Monad_infix = {
    let (>>=) = (m, f) => bind(m, ~f);
    let (>|=) = (m, f) => map(m, ~f);
  };
};
