open%shared Containers

module%shared StringSet = Set.Make(struct type t = string let compare = compare end)

module Feedback_app =
  Eliom_registration.App (struct
    let application_name = "feedback"
    let global_data_path = None
  end)

[%%shared
  type user_vars =
    | Done
    | NeedHelp
  [@@deriving json]

  type notify_details =
    | ButtonChange of string * user_vars * bool
    | ClearAllDone
  [@@deriving json]

  let user_vars_to_string = function
    | Done -> "Done"
    | NeedHelp -> "NeedHelp"

  let notify_details_to_string = function
    | ButtonChange(s, u, b) ->
      Printf.sprintf "ButtonChange(%s, %a, %s)" s (fun () -> user_vars_to_string) u
        (if b then "true" else "false")
    | ClearAllDone -> "ClearAllDone"
]

(* events from server -> admin/student clients *)
let admin_push_event, admin_push_event_send = React.E.create ()
let student_push_event, student_push_event_send = React.E.create ()

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

(* student client button behavior *)
let%client switch_color elt state =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  let grey = Js.string "grey" in
  let pressed = not @@ Js.to_bool @@ elt##.classList##contains grey in
  if not state && pressed then
    elt##.classList##add grey
  else if state && not pressed then
    elt##.classList##remove grey
  else ()

(* admin status widget *)
let%shared status_widget name buttons = Eliom_content.Html.D.(
  let extra = if List.mem Done buttons then [] else ["grey"] in
  let done_btn =
    div ~a:[a_class @@ ["status_button"; "done"] @ extra] [pcdata ""]
  in
  let extra = if List.mem NeedHelp buttons then [] else ["grey"] in
  let help_btn =
    div ~a:[a_class @@ ["status_button"; "help"] @ extra] [pcdata ""]
  in
  let e =
    div ~a:[a_class ["student-status"]] [
      done_btn; help_btn; div [pcdata name];
    ]
  in
  name, e, done_btn, help_btn
)

(* active users -> [user_var] *)
let user_table = Hashtbl.create 50

(* server receive student button toggle *)
let%server notify_server data =
  (* Lwt_io.printf "notify_server: %s\n" (notify_details_to_string data); *)
  let () = match data with
    | ButtonChange (name, button, state) ->
        let buttons = Hashtbl.find user_table name in
        let new_buttons =
          if state then button::buttons
          else List.filter ((<>) button) buttons
        in
        Hashtbl.replace user_table name new_buttons;
        (* notify the admin client *)
        admin_push_event_send data
        (* student_push_event_send data *)
    | ClearAllDone ->
        (* Clear done buttons *)
        Hashtbl.filter_map_inplace
          (fun k l -> Some(List.filter ((<>) Done) l)) user_table;
        Lwt.async (fun () -> student_push_event_send data; Lwt.return ());
        Lwt.async (fun () -> admin_push_event_send data; Lwt.return ());
        ()
  in
  Lwt.return ()

(* stub for student client->server RPC *)
let%client notify_server =
  ~%(Eliom_client.server_function [%derive.json: notify_details] notify_server)

(* student button widget *)
let%client init_student_client name (done_btn, help_btn) event =
  let buttons = [|done_btn; help_btn|] in
  let state = [|false; false|] in
  Eliom_content.Html.D.(
    let react_to_event = function
      | ClearAllDone ->
        state.(0) <- false;
        switch_color buttons.(0) false
      | _ -> ()
    in
    let react_click idx () =
        (* react to js clicks *)
        Lwt_js_events.clicks
          (Eliom_content.Html.To_dom.of_element buttons.(idx))
          (fun _ _ ->
            state.(idx) <- not state.(idx);
            switch_color buttons.(idx) state.(idx);
            (* change button state on server *)
            Lwt.async (fun () ->
              let button = match idx with
                | 0 -> Done | 1 -> NeedHelp | _ -> failwith "wrong num"
              in
              notify_server @@
                ButtonChange(name, button, state.(idx)));
            Lwt.return ())
    in
    Lwt.async (react_click 0);
    Lwt.async (react_click 1);
    let _ = React.E.map react_to_event event in
    ()
  )

(* student service *)
let _ = Eliom_content.Html.D.(
  let event = Eliom_react.Down.of_react student_push_event in
  let done_btn =
    div ~a:[a_class ["button"; "done"; "grey"]] [pcdata "Done Working"] in
  let help_btn =
    div ~a:[a_class ["button"; "help"; "grey"]] [pcdata "I Need Help"] in
  Feedback_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.(suffix @@ string "name"))
    (fun name () ->
       let page =
         (* check if user is valid *)
         if StringSet.mem name user_set then begin
           (* add button to hashtable *)
           if not @@ Hashtbl.mem user_table name
           then Hashtbl.replace user_table name [] else ();
           (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata @@ "Hello "^name];
                done_btn;
                help_btn;
              ]))
         end else
            (Eliom_tools.D.html ~title:"Forbidden" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata "Forbidden access"];
              ]))
       in
       let _ =
         [%client (init_student_client
                     ~%name (~%done_btn, ~%help_btn) ~%event : unit)]
       in
       Lwt.return page
    ))

(* admin client routine *)
let%client init_admin_client clear_button status_widget_elems push_event =
  (* save the widgets *)
  let widget_index = Hashtbl.create 50 in
  List.iter (fun (nm, _, don, help) ->
    Hashtbl.replace widget_index nm (don,help))
    status_widget_elems;
  (* update a widget via an event *)
  let update_widget = function
    | ButtonChange(name, button, state) ->
        let (don, help) = Hashtbl.find widget_index name in
        let elt = match button with
          | Done -> don
          | NeedHelp -> help
        in
        let elt = Eliom_content.Html.To_dom.of_element elt in
        let grey = Js.string "grey" in
        if state then
          elt##.classList##remove grey
        else
          elt##.classList##add grey
    | ClearAllDone ->
      Hashtbl.iter (fun _ (done_btn, _) ->
        let elt = Eliom_content.Html.To_dom.of_element done_btn in
        elt##.classList##add (Js.string "grey")
      ) widget_index
  in
  let react_clear_click () =
    Lwt_js_events.clicks
      (Eliom_content.Html.To_dom.of_element clear_button)
      (fun _ _ ->
         Lwt.async (fun () -> notify_server ClearAllDone);
         Lwt.return ())
  in
  Lwt.async react_clear_click;
  let _ = React.E.map update_widget push_event in
  ()

(* admin service *)
let _ = Eliom_content.Html.D.(
  let event = Eliom_react.Down.of_react admin_push_event in
  let clear_button = div ~a:[a_class ["clear_button"]] [pcdata "Clear All"] in
  Feedback_app.create
    ~path:(Eliom_service.Path ["admin"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
      (* create elems to match active users, status *)
      let elems = List.filter_map (fun nm ->
        match CCHashtbl.get user_table nm with
        | None -> None
        | Some l -> Some (status_widget nm l))
        user_list in
      let status_widgets = List.map (fun (_,x,_,_) -> x) elems in
      let page =
        (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
           (body (
              (h2 [pcdata "Welcome to the admin page"])::
              clear_button::
              status_widgets)
           ))
       in
       let _ = [%client (init_admin_client ~%clear_button ~%elems ~%event: unit) ]
       in
       Lwt.return page
    ))

