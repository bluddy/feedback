module Feedback_app =
  Eliom_registration.App (struct
    let application_name = "feedback"
    let global_data_path = None
  end)

let%client switch_color elt =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  let pressed = Js.string "pressed" in
  if Js.to_bool @@ elt##.classList##contains pressed then
    elt##.classList##remove pressed
  else
    elt##.classList##add pressed

let%shared button_widget s1 button_class = Eliom_content.Html.D.(
  let button = div ~a:[a_class ["button"]] [pcdata s1] in
  let _ = [%client
    (Lwt.async (fun () ->
      Lwt_js_events.clicks
        (Eliom_content.Html.To_dom.of_element ~%button)
        (fun _ _ ->
           switch_color ~%button;
           Lwt.return ()
        ))
     : unit)
  ] in
  div ~a:[a_class ["button_widget"; button_class]] [button]
)

let%shared status_widget name = Eliom_content.Html.D.(
  div ~a:[a_class ["student-status"]] [
    div ~a:[a_class ["status_button"; "done"]] [pcdata ""];
    div ~a:[a_class ["status_button"; "help"]] [pcdata ""];
    div [pcdata name];
  ]
)

module StringSet = Set.Make(struct type t = string let compare = compare end)

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

(* user service *)
let _ = Eliom_content.Html.D.(
  Feedback_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.(suffix @@ string "name"))
    (fun name () ->
       let page =
         if StringSet.mem name user_set then
           (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
              (body [
                h2 [pcdata @@ "Hello "^name];
                button_widget "Done Working" "done";
                button_widget "I Need Help" "help";
              ]))
         else
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

