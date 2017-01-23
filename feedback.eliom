module%shared StringSet = Set.Make(struct type t = string let compare = compare end)

module Feedback_app =
  Eliom_registration.App (struct
    let application_name = "feedback"
    let global_data_path = None
  end)

let%client switch_color elt =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  let pressed = Js.string "grey" in
  if Js.to_bool @@ elt##.classList##contains pressed then
    elt##.classList##remove pressed
  else
    elt##.classList##add pressed

let%shared status_widget name = Eliom_content.Html.D.(
  div ~a:[a_class ["student-status"]] [
    div ~a:[a_class ["status_button"; "done"]] [pcdata ""];
    div ~a:[a_class ["status_button"; "help"]] [pcdata ""];
    div [pcdata name];
  ]
)

let user_list =
  let file = open_in "users.txt" in
  let rec read () =
    try
      let l = input_line file in
      l::read ()
    with End_of_file -> []
  in
  let s = read () in
  close_in file; s

let user_set = StringSet.of_list user_list

[%%shared
  type user_vars =
    | Done
    | Need_help
  [@@deriving json]
]

(* users -> user_vars *)
let user_table = Hashtbl.create 50

(* receive button toggle *)
let%server toggle_button (name, button, state) =
  (* Lwt_io.printf "%s, %s, %s\n" name (match button with Done -> "done" | Need_help -> "help") (if state then "true" else "false"); *)
  let buttons = Hashtbl.find user_table name in
  let new_buttons =
    if state then button::buttons
    else List.filter (fun b -> b <> button) buttons
  in
  Hashtbl.replace user_table name new_buttons;
  Lwt.return ()

(* stub for client *)
let%client toggle_button =
  ~%(Eliom_client.server_function [%derive.json: string * user_vars * bool] toggle_button)

let%shared button_widget name s color_class = Eliom_content.Html.D.(
  let button = div ~a:[a_class ["button"; color_class; "grey"]] [pcdata s] in
  let _ = [%client
    (let state = ref false in
      Lwt.async (fun () ->
      Lwt_js_events.clicks
        (Eliom_content.Html.To_dom.of_element ~%button)
        (fun _ _ ->
           state := not !state;
           switch_color ~%button;
           (* change button state on server *)
           Lwt.async (fun () ->
             toggle_button (~%name, (if ~%color_class = "done" then Done else Need_help), !state));
           Lwt.return ()
        ))
     : unit)
  ] in
  button
)

(* user service *)
let _ = Eliom_content.Html.D.(
  Feedback_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.(suffix @@ string "name"))
    (fun name () ->
       let page =
         if StringSet.mem name user_set then begin
           (* add to hashtable *)
           if not @@ Hashtbl.mem user_table name
           then Hashtbl.replace user_table name [] else ();
           (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata @@ "Hello "^name];
                button_widget name "Done Working" "done";
                button_widget name "I Need Help" "help";
              ]))
         end else
           (Eliom_tools.D.html ~title:"Forbidden" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata "Forbidden access"];
              ]))
         in
         Lwt.return page
    )
)

(* admin service *)
let _ = Eliom_content.Html.D.(
  let status_widgets = List.map status_widget user_list in
  Feedback_app.create
    ~path:(Eliom_service.Path ["admin"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
       Lwt.return
         (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
            (body (
               (h2 [pcdata "Welcome to the admin page"])::
               status_widgets)
             ))))

