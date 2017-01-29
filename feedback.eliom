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
    | LogOut
  [@@deriving json]

  let user_vars_to_string = function
    | Done -> "Done"
    | NeedHelp -> "NeedHelp"

  let notify_details_to_string = function
    | ButtonChange(s, u, b) ->
      Printf.sprintf "ButtonChange(%s, %a, %s)" s (fun () -> user_vars_to_string) u
        (if b then "true" else "false")
    | ClearAllDone -> "ClearAllDone"
    | LogOut -> "LogOut"
]

(* events from server -> admin/student clients *)
let admin_push_event, admin_push_event_send = React.E.create ()
let student_push_event, student_push_event_send = React.E.create ()

let user_list =
  let file = open_in "users.txt" in
  let rec read () =
    try
      let l = input_line file in
      (* comma separated: full_name, url_name *)
      match String.Split.list_cpy ~by:"," l with
      | x::[y] -> (y,x)::read () (* notice flip *)
      | _ -> read ()
    with End_of_file -> []
  in
  let s = read () in
  close_in file; s

(* fast access *)
let user_tbl = CCHashtbl.of_list user_list

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
let%shared status_widget url_name full_name buttons = Eliom_content.Html.D.(
  let extra = if List.mem Done buttons then [] else ["grey"] in
  let done_btn =
    div ~a:[a_class @@ ["status_button"; "done"] @ extra] [pcdata ""]
  in
  let extra = if List.mem NeedHelp buttons then [] else ["grey"] in
  let help_btn =
    div ~a:[a_class @@ ["status_button"; "help"] @ extra] [pcdata ""]
  in
  let short_name =
    match String.Split.list_cpy ~by:" " full_name with
    | [nm]    -> nm
    | nm::nms ->
      Printf.sprintf "%s %c" nm @@ (List.hd nms).[0]
    | _       -> ""
  in
  let e =
    div ~a:[a_class ["student-status"]] [
      done_btn; help_btn; div [pcdata short_name];
    ]
  in
  url_name, e, done_btn, help_btn
)

(* active users -> [user_var] *)
let active_table = Hashtbl.create 50

(* server receive student button toggle *)
let%server notify_server data =
  (* Lwt_io.printf "notify_server: %s\n" (notify_details_to_string data); *)
  let () = match data with
    | ButtonChange (url_name, button, state) ->
        let buttons = match CCHashtbl.get active_table url_name with
          | Some l -> l | None -> []
        in
        let new_buttons =
          if state then button::buttons
          else List.filter ((<>) button) buttons
        in
        Hashtbl.replace active_table url_name new_buttons;
        (* notify the admin client *)
        admin_push_event_send data
        (* student_push_event_send data *)
    | ClearAllDone ->
        (* Clear done buttons *)
        Hashtbl.filter_map_inplace
          (fun k l -> Some(List.filter ((<>) Done) l)) active_table;
        Lwt.async (fun () -> student_push_event_send data; Lwt.return ());
        Lwt.async (fun () -> admin_push_event_send data; Lwt.return ());
        ()
    | LogOut ->
        (* Remove all people from active_table *)
        Hashtbl.clear active_table
  in
  Lwt.return ()

(* stub for student client->server RPC *)
let%client notify_server =
  ~%(Eliom_client.server_function [%derive.json: notify_details] notify_server)

(* student button widget *)
let%client init_student_client url_name (done_btn, help_btn) event =
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
                ButtonChange(url_name, button, state.(idx)));
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
  let btn nm lbl pressed =
    let extra = if pressed then [] else ["grey"] in
    div ~a:[a_class @@ ["button"; nm] @ extra] [pcdata lbl]
  in
  let help_btn pressed = btn "help" "I Need Help" pressed in
  let done_btn pressed = btn "done" "Done Working" pressed in
  Feedback_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.(suffix @@ string "url_name"))
    (fun url_name () ->
       let page =
         (* check if user is valid *)
         match CCHashtbl.get user_tbl url_name with
         | Some full_name ->
            let done_btn, help_btn =
             (* add button to active_table - also indicates usage *)
              begin match CCHashtbl.get active_table url_name with
              | None ->
                  Hashtbl.replace active_table url_name [];
                  done_btn false, help_btn false
              | Some l ->
                  done_btn (List.mem Done l), help_btn (List.mem NeedHelp l)
              end
            in
            (* init student client *)
            let _ =
              [%client (init_student_client
                          ~%url_name (~%done_btn, ~%help_btn) ~%event : unit)]
            in
            (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
                (body [
                  h2 [pcdata @@ "Hello "^full_name];
                  done_btn;
                  help_btn;
                ]))
          (* not a valid user *)
         | None ->
            (Eliom_tools.D.html ~title:"Forbidden" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata "Forbidden access"];
              ]))
       in
       Lwt.return page
    ))

(* admin client routine *)
let%client init_admin_client clear_button logout_btn status_widget_elems push_event =
  (* save the widgets *)
  let widget_index = Hashtbl.create 50 in
  List.iter (fun (nm, _, don, help) ->
    Hashtbl.replace widget_index nm (don,help))
    status_widget_elems;
  (* update a widget via an event *)
  let update_widget = function
    | ButtonChange(url_name, button, state) ->
        let (don, help) = Hashtbl.find widget_index url_name in
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
    | LogOut -> ()
  in
  Lwt.async (fun () ->
    Lwt_js_events.clicks
      (Eliom_content.Html.To_dom.of_element clear_button)
      (fun _ _ ->
         Lwt.async (fun () -> notify_server ClearAllDone);
         Lwt.return ()));
  Lwt.async (fun () ->
    Lwt_js_events.clicks
      (Eliom_content.Html.To_dom.of_element logout_btn)
      (fun _ _ ->
         Lwt.async (fun () -> notify_server LogOut);
         Lwt.return ()));
  let _ = React.E.map update_widget push_event in
  ()

(* admin service *)
let _ = Eliom_content.Html.D.(
  let event = Eliom_react.Down.of_react admin_push_event in
  let clear_button = div ~a:[a_class ["clear_button"]] [pcdata "Clear All"] in
  let logout_btn = div ~a:[a_class ["clear_button"]] [pcdata "Log Out"] in
  Feedback_app.create
    ~path:(Eliom_service.Path ["admin"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
      (* create elems to match active users, status *)
      let elems = List.filter_map (fun (url_name,full_name) ->
        match CCHashtbl.get active_table url_name with
        | None -> None
        | Some l -> Some (status_widget url_name full_name l))
        user_list in
      let status_widgets = List.map (fun (_,x,_,_) -> x) elems in
      let page =
        (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
           (body (
              (h2 [pcdata "Welcome to the admin page"])::
              clear_button::
              status_widgets@
              [logout_btn]
            )
           ))
       in
       let _ = [%client (init_admin_client ~%clear_button ~%logout_btn
                           ~%elems ~%event: unit) ]
       in
       Lwt.return page
    ))

