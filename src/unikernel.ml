(* This code implements a minimal kernel that responds to
 * life cycle events.
 *)

(* Documentation of important interfaces:
 * http://mirage.github.io/mirage-xen/#Xs
 * http://mirage.github.io/mirage-xen/#Sched
 * http://mirage.github.io/mirage-types/#V1:CONSOLE
 * http://mirage.github.io/xenstore/#Xs_protocol
 *)

module Main (Time : Mirage_time.S) = struct
  module CMD = Commands
  module Main = Xen_os.Main
  module XS = Xen_os.Xs

  let ( let* ) = Lwt.bind
  let return = Lwt.return
  let ( let@ ) f x = f x

  let read client path =
    let@ h = XS.immediate client in
    XS.read h path

  (* [ack] acknowledges a message and offers to violate the proper
     protocol (AckOK) by doing something else *)
  let ack client path = function
    | CMD.AckOK ->
        let@ h = XS.immediate client in
        XS.write h path ""
    | CMD.AckWrite x ->
        let@ h = XS.immediate client in
        XS.write h path x
    | CMD.AckDelete ->
        let@ h = XS.immediate client in
        XS.rm h path
    | CMD.AckNone -> return ()

  (* [read_opt client path] reads [path] from the Xen Store and
     returns it as an option value on success, and [None] otherwise.
     Unexpected errors still raise an exception.
  *)
  let read_path client path =
    Lwt.catch
      (fun () ->
        let* msg = read client path in
        return (Some msg))
      (function Xs_protocol.Enoent _ -> return None | ex -> Lwt.fail ex)

  (** [read_cmd] reads a command in JSON format from [path] and
   * returns it, or [None] when nothing is there *)
  let read_cmd client path =
    let* msg = read_path client path in
    let cmd = Option.map Commands.from_string msg in
    let* () =
      if Option.is_some cmd then ack client path CMD.AckOK else return ()
    in
    return cmd

  let suspend name k =
    (* Is there anything to do here? *)
    Logs.info (fun m -> m "%s!" name);
    k ()

  let poweroff name _ =
    Logs.info (fun m -> m "%s!" name);
    (* Shut down the kernel by finishing the stream of promises *)
    return ()

  (** [dispatch] implements the reaction to control messages *)
  let dispatch action k =
    let open CMD.Action in
    let name () = (to_string action) in
    match action with
    | PowerOff -> poweroff (name ()) k
    | Reboot -> poweroff (name ()) k
    | Suspend -> suspend (name ()) k
    | Ignore -> k ()

  let start _time =
    let* client = XS.make () in
    let rec loop tick cmd =
      let next cmd () = loop (tick + 1) cmd in
      let* msg = read_path client CMD.xenstore_action_key in
      (* report the current state *)
      let* () = Time.sleep_ns (Duration.of_sec 1) in
      let* domid = read client "domid" in
      let () = Logs.info (fun m -> m "domain %s tick %d" domid tick) in
      let* () =
        match cmd with
        | Some _ ->
            let () = Logs.info (fun m -> m "command is active") in
            return ()
        | None -> return ()
      in
      match (cmd, msg) with
      | None, None -> loop (tick + 1) cmd
      | None, Some msg ->
          let* () = ack client CMD.xenstore_action_key CMD.AckOK in
          let@ () = CMD.Action.of_string msg |> dispatch in
          next cmd ()
      | Some (CMD.Now action), _ ->
          let@ () = dispatch action in
          next None ()
      | Some (CMD.OnShutdown (a, action)), Some _ ->
          let* () = ack client CMD.xenstore_action_key a in
          let@ () = dispatch action in
          next None ()
      | Some (CMD.OnShutdown (_, _)), None ->
          next None ()
    in
    loop 0 None
end
