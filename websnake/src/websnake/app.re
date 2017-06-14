open Common;

module Top = {
  type state = Model.t;
  let component = ReasonReact.statefulComponent "TOP-ELEM";
  let make _children => {
    let heartbeat _event state self => {
      let state : state = state;
      ReasonReact.Update {...(Model.next_state state), dir_mutex:`Released}
    };
    let handleKeyboard event state self => {
      let state : state = state;
      let new_dir =
        switch (ReactEventRe.Keyboard.which event) {
        | 37 => `Left
        | 38 => `Up
        | 39 => `Right
        | 40 => `Down
        | _  => state.dir
        };
      ReasonReact.Update {...state, dir:new_dir }
    };
    { ...component,
      initialState: fun () => Model.initial_state,
      didMount: fun state self => {
        let state : state = state;
        let timeoutId = Some (Js.Global.setInterval (self.update heartbeat) 100);
        ReasonReact.Update {...state, timeoutId:timeoutId}
      },
      render: fun state self => {
        /* wtf? why do I need to cast this type? */
        let state : state = state;
        let alive = string_of_bool state.is_alive;
        let (w, h) = state.dim;
        let cell_width = 25;
        let cells = {
          let (rows, cols) = state.dim;
          let string_of_cell_type t =>
            switch t {
            | `Snake => "snake"
            | `Apple => "apple"
            | `Void  => "void"
            };

          let to_cell state t => {
            let state : state = state;
            let color =
              switch t {
              | `Snake =>
                switch state.is_alive {
                | true  => "#00ff00"
                | false => "#ff0000"
                };
              | `Apple => "#ff8800"
              | `Void  => "#000000"
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
                         height::((string_of_int cell_width) ^ "px")
                         display::"table-row"
                         ())>
                    (row
                     |> List.map f::(to_cell state)
                     |> Array.of_list
                     |> ReasonReact.arrayToElement)
                  </div>
                }
              )
        };
        <div>
          <input onKeyDown=(self.update handleKeyboard)
                 autoFocus=(Js.true_)
                 />
          (ReasonReact.arrayToElement (Array.of_list cells))
        </div>
      }
    };
  };
};

ReactDOMRe.renderToElementWithClassName <Top /> "websnake_game";
