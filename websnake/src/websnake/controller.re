open Common;

open Lwt.Infix;

/* type game_event = [ `Play | `End ]; */

let render {Model.snake: snake, is_alive, apple} ::dim => {
  ()
};

let start_refresh_timer () => Lwt.return `End;

let read_event event_stream =>
  Lwt_stream.get event_stream >|= (
    fun
    | Some _ => `Play
    | None => `End
  );

type event_producer 'a = {
  event: Lwt.t 'a,
  refresh: Lwt.t 'a
};

let pop_event {event, refresh} => event <?> refresh;

type state = Model.t;

type event_handler 'a = 'a => state => Lwt.t state;

let handle_noop: event_handler 'a = fun event state => Lwt.return state;

let handle_refresh: event_handler 'a =
  /* fix this */
  fun event state => {
    Lwt.return state
  };

let handle_keypress dir lock_dir : event_handler 'a =>
  fun event state =>
    switch (not lock_dir && state.is_alive) {
    | false => handle_noop event state
    | true => Lwt.return (Model.change_direction state dir)
    };

let handle_resize dim : event_handler 'a => fun event state => Lwt.return {...state, dim};

let rec event_loop ::lock_dir=false event_stream events state =>
  /* Process the first timer that completes */
  pop_event events >>= (
    fun
    | `End
    | `Key (`Escape, [])
    | `Key (`Uchar 67, [`Ctrl]) =>
      /* The user has requested to exit the application */
      Lwt.return_unit
    | `Refresh as event =>
      /* The refresh timer has expired so we must render the
       * to the screen and reset the refresh timer. */
      handle_refresh event state >>= (
        fun new_state => {
          let events = {...events, refresh: start_refresh_timer ()};
          event_loop event_stream events new_state
        }
      )
    | `Key (`Arrow dir, []) as event when state.is_alive && not lock_dir =>
      /* A control event has occurred, we must lock the
       * system until the next refresh event */
      handle_keypress dir lock_dir event state >>= (
        fun new_state => {
          let events = {...events, event: read_event event_stream};
          event_loop event_stream events new_state lock_dir::true
        }
      )
    | `Resize dim => {
        /* A layout change event has occurred, we can just
         * re-render and continue without locking */
        let new_state = {...state, dim};
        let events = {...events, event: read_event event_stream};
        event_loop event_stream events new_state
      }
    | _ as event =>
      /* We don't know how to handle this event, so just
       * reset the event timer */
      handle_noop event state >>= (
        fun new_state => {
          let events = {...events, event: read_event event_stream};
          event_loop event_stream events new_state
        }
      )
  );

let start_event_loop () => {
  let tc = Unix.(tcgetattr stdin);
  Unix.(tcsetattr stdin TCSANOW {...tc, c_isig: false});
  let size = (10, 10);
  let state = {...Model.initial_state, dim: size, apple: (10, 10)};
  let event_stream = Lwt_stream.from (fun () => Lwt.return (Some `Play));
  let events = {event: read_event event_stream, refresh: start_refresh_timer ()};
  event_loop event_stream events state
};
