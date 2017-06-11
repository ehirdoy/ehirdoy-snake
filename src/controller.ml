open Common
open Lwt.Infix
module Term = Notty_lwt.Term

type game_event = [
  | Notty.Unescape.event
  | `Resize of int * int
  | `End
  | `Refresh
]

let render { Model.snake; is_alive } ~dim =
  let open Notty in
  let open Notty.Infix in
  let w, h = dim in
  range 0 (h - 2) |> List.map ~f:(fun m ->
    range 0 (w - 2) |> List.map ~f:(fun n ->
      let pt = (n, m) in
      if Snake.mem snake ~point:pt then
        View.snake_color is_alive
      else
        View.background pt ~dim
    ) |> I.hcat
  ) |> I.vcat
  <->
  let (x,y) = List.hd snake in
  I.(strf ~attr:A.(fg lightblack) "%d %d" x y |> hsnap ~align:`Middle w)

let start_refresh_timer () =
  Lwt_unix.sleep (1.0 /. 60.0) >|= fun () -> `Refresh

let read_event (term:Term.t) : game_event Lwt.t=
  Lwt_stream.get (Term.events term) >|= function
  | Some (`Resize _ | #Notty.Unescape.event as x) -> x
  | None -> `End

type event_producer = {
  event: game_event Lwt.t;
  refresh: game_event Lwt.t;
}

let pop_event {event; refresh} =
  event <?> refresh

let rec event_loop ?(lock_dir = false) term events (state : Model.t) =
  (* Process the first timer that completes *)
  pop_event events
  >>= function
  | `End | `Key (`Escape, []) | `Key (`Uchar 67, [`Ctrl]) ->
    (* The user has requested to exit the application *)
    Lwt.return_unit

  | `Refresh ->
    (* The refresh timer has expired so we must render the
     * to the screen and reset the refresh timer. *)
    let rendered = render state ~dim:state.dim in
    Term.image term rendered
    >>= fun () ->
    let new_state = Model.next_state state in
    let events = { events with refresh = start_refresh_timer () } in
    event_loop term events new_state

  | `Key (`Arrow dir, []) when state.is_alive && (not lock_dir) ->
    (* A control event has occurred, we must lock the
     * system until the next refresh event *)
    let new_state = Model.change_direction state dir in
    let events = { events with event = read_event term } in
    event_loop term events new_state ~lock_dir:true

  | `Resize dim ->
    (* A layout change event has occurred, we can just
     * re-render and continue without locking *)
    let new_state = { state with dim = dim } in
    let events = { events with event = read_event term } in
    event_loop term events new_state

  | _ ->
    (* We don't know how to handle this event, so just
     * reset the event timer *)
    let new_state = state in
    let events = { events with event = read_event term } in
    event_loop term events new_state

let start_event_loop () =
  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false });
  let term = Term.create () in
  let size = Term.size term in
  let state = { Model.initial_state with dim = size } in
  let events = {
    event = read_event term;
    refresh = start_refresh_timer ();
  } in
  event_loop term events state

let start () = Lwt_main.run @@ start_event_loop ()
