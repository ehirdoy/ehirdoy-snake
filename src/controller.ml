open Common
open Lwt.Infix
module Term = Notty_lwt.Term

let render { Model.snake; is_alive } ~dim =
  let open Notty in
  let open Notty.Infix in
  let w, h = dim in
  0 --> (h - 2) |> List.map ~f:(fun m ->
    0 --> (w - 2) |> List.map ~f:(fun n ->
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

let start_timer () = Lwt_unix.sleep (1.0 /. 60.0) >|= fun () -> `Timer

let read_event term =
  Lwt_stream.get (Term.events term) >|= function
  | Some (`Resize _ | #Notty.Unescape.event as x) -> x
  | None -> `End

let rec loop ?(lock_dir = false) term (event, timer) (state : Model.t) =
  (event <?> timer) >>= function
  | `End | `Key (`Escape, []) | `Key (`Uchar 67, [`Ctrl]) ->
    Lwt.return_unit
  | `Timer ->
    Term.image term (render state ~dim:state.dim) >>= fun () ->
    loop term (event, start_timer ()) (Model.next_state state)
  | `Key (`Arrow dir, []) when state.is_alive && (not lock_dir) ->
    let state = Model.change_dir state dir in
    loop term (read_event term, timer) state ~lock_dir:true
  | `Resize dim ->
    let state = { state with dim = dim } in
    loop term (read_event term, timer) state
  | _ ->
    loop term (read_event term, timer) state

let main () =
  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false });
  let term  = Term.create () in
  let size  = Term.size term in
  let state = { Model.initial_state with dim = size } in
  loop term (read_event term, start_timer ()) state

let start () = Lwt_main.run @@ main ()
