open Common;

module Top = {
  type state = Model.t;
  let component = ReasonReact.statefulComponent "TOP-ELEM";
  let make _children => {
    let random_pos state => {
      let state: state = state;
      let (width, height) = state.dim;
      let rx = Random.int (width - 3 + 1);
      let ry = Random.int (height - 3 + 1);
      (rx, ry)
    };
    let heartbeat _event {ReasonReact.state: state} => {
      let state: state = state;
      state.running ?
        ReasonReact.Update {...Model.next_state state, dir_mutex: `Released} : ReasonReact.NoUpdate
    };
    let handleKeyboard event {ReasonReact.state: state} => {
      let state: state = state;
      let key_pressed =
        switch (ReactEventRe.Keyboard.which event) {
        | 32 => `Space
        | 37 => `Left
        | 38 => `Up
        | 39 => `Right
        | 40 => `Down
        | _ => `Unknown
        };
      let game_event =
        switch (state.is_alive, state.running) {
        | (true, true) => `Change_direction
        | (true, false) => `Start
        | (false, true) => `Stop
        | (false, false) => `Check_for_space
        };
      let new_state = {
        let new_dir =
          switch key_pressed {
          | (`Left | `Right | `Down | `Up) as x => x
          | _ => state.dir
          };
        switch game_event {
        | `Start => {...state, dir: new_dir, running: true}
        | `Stop => {...state, running: false}
        | `Change_direction => Model.change_direction state new_dir
        | `Check_for_space =>
          switch key_pressed {
          | `Space =>
            let snake = Snake.make len::1 pos::(random_pos state);
            let apple = state.apple;
            {...Model.initial_state, apple, snake, running: false}
          | _ => state
          }
        }
      };
      ReasonReact.Update new_state
    };
    {
      ...component,
      initialState: fun () => {
        Random.self_init ();
        let snake = Snake.make len::1 pos::(random_pos Model.initial_state);
        let apple = random_pos Model.initial_state;
        {...Model.initial_state, snake, apple}
      },
      didMount: fun ({state} as self) => {
        let state: state = state;
        let timeoutId = Some (Js.Global.setInterval (self.update heartbeat) 66);
        ReasonReact.Update {...state, timeoutId}
      },
      render: fun ({state} as self) => {
        let state: state = state;
        let (w, h) = state.dim;
        let cell_width = 25;
        let cells = {
          let (rows, _) = state.dim;
          let string_of_cell_type t =>
            switch t {
            | `Snake => "snake"
            | `Apple => "apple"
            | `Void => "void"
            };
          let to_cell state t => {
            let state: state = state;
            let color =
              switch t {
              | `Snake => state.is_alive ? "#b8bb26" : "#fb4934"
              | `Apple => "#fe8019"
              | `Void => "#1d2021"
              };
            <div
              className=(Js.String.make (string_of_cell_type t ^ "-cell"))
              style=(
                ReactDOMRe.Style.make
                  width::(string_of_int cell_width ^ "px")
                  height::(string_of_int cell_width ^ "px")
                  backgroundColor::color
                  display::"table-cell"
                  ()
              )
            />
          };
          range 0 (h - 2) |>
          List.map
            f::(
              fun m =>
                range 0 (w - 2) |>
                List.map
                  f::(
                    fun n => {
                      let pt = (n, m);
                      let (ax, ay) = state.apple;
                      if (Snake.mem state.snake point::pt) {
                        `Snake
                      } else if (ax == n && ay == m) {
                        `Apple
                      } else {
                        `Void
                      }
                    }
                  )
            ) |>
          List.map
            f::(
              fun row =>
                <div
                  className=(Js.String.make "gridRow")
                  style=(
                    ReactDOMRe.Style.make
                      width::(string_of_int (cell_width * rows) ^ "px")
                      height::(string_of_int cell_width ^ "px")
                      display::"table-row"
                      ()
                  )>
                  (
                    row |> List.map f::(to_cell state) |> Array.of_list |> ReasonReact.arrayToElement
                  )
                </div>
            )
        };
        let message =
          switch (state.running, state.is_alive) {
          | (true, true) => "Score: " ^ string_of_int state.score
          | (false, true) => "Press any ARROW key to begin!"
          | (_, false) => "Rip... Press SPACE to start over!"
          };
        <div>
          <div
            style=(
              ReactDOMRe.Style.make
                color::"#fb4934" fontFamily::"Helvetica Neue, Arial" fontWeight::"bold" ()
            )>
            (ReasonReact.stringToElement message)
          </div>
          <input
            onKeyDown=(self.update handleKeyboard)
            autoFocus=Js.true_
            style=(
              ReactDOMRe.Style.make
                width::"0px" height::"0px" position::"relative" top::"100px" zIndex::"-100" ()
            )
          />
          (ReasonReact.arrayToElement (Array.of_list cells))
        </div>
      }
    }
  };
};

ReactDOMRe.renderToElementWithClassName <Top /> "websnake_game";