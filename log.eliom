open Containers

let write name str =
  Ocsigen_messages.accesslog @@
    Printf.sprintf "%s %s\n" name str

let async_write name str =
  Lwt.async (fun () -> write name str; Lwt.return ())
