module Feedback_app =
  Eliom_registration.App (struct
    let application_name = "feedback"
    let global_data_path = None
  end)

let new_set () = [%client (ref false : bool ref)]

let%shared green, red = "#51D23A", "#BA2A1C"

let%client switch_color elt =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  let pressed = Js.string "pressed" in
  if Js.to_bool @@ elt##.classList##contains pressed then
    elt##.classList##remove pressed
  else
    elt##.classList##add pressed

let%shared button_widget set s1 button_class = Eliom_content.Html.D.(
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

let _ = Eliom_content.Html.D.(
  Feedback_app.create
    ~path:(Eliom_service.Path [""])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
       let set1 = new_set () in
       let set2 = new_set () in
       Lwt.return
         (Eliom_tools.D.html ~title:"Feedback" ~css:[["css"; "feedback.css"]]
            (body [
               h2 [pcdata "Welcome to CS120"];
               button_widget set1 "Done Working" "done";
               button_widget set2 "I Need Help" "help";
             ])))
)

