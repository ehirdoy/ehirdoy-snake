open Common;

external setInterval : (unit => unit) => int => int = "window.setInterval" [@@bs.val];

module Top = {
  type state = Model.t;
  let component = ReasonReact.statefulComponent "TOP-ELEM";
  /*
  let heartbeat state self => {
    let state : state = state;
    let new_snake =
      switch (Snake.move state.snake dir::state.dir dim::state.dim) {
      | Some snake => snake
      | None => state.snake
      };
    {...state, snake: new_snake }
  };
  */
  let make _children => {
    ...component,
    initialState: fun () => {
      let dir = `Up;
      let snake = Snake.make len::10;
      let apple = (10, 10);
      let is_alive = true;
      let dim = (20, 20);
      /* wtf? why do I need to cast this type? */
      let state : state =
        { snake, dir, apple, is_alive, dim, timeoutId:(Some (setInterval (fun () => ()) 3000))};
      state

    },
    /*
    didMount: fun state self => {
      let state : state = state;
      let timeoutId = setInterval (fun () => ReasonReact.Update heartbeat) 3000;
      ReasonReact.SilentUpdate {...state, timeoutId:timeoutId}
    },
    */
    render: fun state self => {
      /* wtf? why do I need to cast this type? */
      let state : state = state;
      let alive = string_of_bool state.is_alive;
      let (w, h) = state.dim;
      let cell_width = 50;
      let cells = {
        let (rows, cols) = state.dim;
        let string_of_cell_type t =>
          switch t {
          | `Snake => "snake"
          | `Apple => "apple"
          | `Void  => "void"
          };

        let to_cell t => {
          let color =
            switch t {
            | `Snake => "#00ff00"
            | `Apple => "#ff0000"
            | `Void  => "#222222"
            };

          <div className=(Js.String.make ((string_of_cell_type t) ^ "-cell"))
               style=(ReactDOMRe.Style.make
                 width::((string_of_int cell_width) ^ "px")
                 height::((string_of_int cell_width) ^ "px")
                 backgroundColor::color
                 display::"table-cell"
               ())/>
        };
        range 0 (h - 2) |> List.map f::(fun m => {
          range 0 (w - 2) |> List.map f::(fun n => {
            let pt = (n, m);
            let (ax, ay) = state.apple;
            if (Snake.mem state.snake point::pt) {
              `Snake
            } else if (ax == n && ay == m) {
              `Apple
            } else {
              `Void
            }
          })
        })
        |> List.map
            f::(
              fun row => {
                <div className=(Js.String.make "gridRow")
                     style=(ReactDOMRe.Style.make
                       width::((string_of_int (cell_width * rows)) ^ "px")
                       height::"50px"
                       display::"table-row"
                       ())>
                  (row
                   |> List.map f::to_cell
                   |> Array.of_list
                   |> ReasonReact.arrayToElement)
                </div>
              }
            )
      };
      <div>
        <header>
          <h1> (ReasonReact.stringToElement alive) </h1>
        </header>
        (ReasonReact.arrayToElement (Array.of_list cells))
      </div>
    }
  };
};

ReactDOMRe.renderToElementWithClassName <Top /> "websnake_game";
