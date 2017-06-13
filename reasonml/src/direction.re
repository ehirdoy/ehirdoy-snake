type t = [`Up | `Down | `Left | `Right];

let opp_dir dir => {
  switch dir {
  | `Up => `Down
  | `Down => `Up
  | `Left => `Right
  | `Right => `Left
  };
};
