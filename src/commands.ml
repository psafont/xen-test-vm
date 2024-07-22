(* vim: set et sw=2 ts=2 *)

module Y  = Yojson.Basic
module U  = Yojson.Basic.Util

(* From http://xenbits.xen.org/docs/unstable/misc/xenstore-paths.html:

1.3.4.2 ~/control/shutdown = (""|COMMAND) [w]
  This is the PV shutdown control node. A toolstack can write various commands here to cause various guest shutdown, reboot or suspend activities. The guest acknowledges a request by writing the empty string back to the command node.
  The precise protocol is not yet documented.

1.3.4.3 ~/control/feature-poweroff = ("“|”0“|”1“) [w] #### ~/control/feature-reboot = (”“|”0“|”1“) [w] #### ~/control/feature-suspend = (”“|”0“|”1") [w]

  These may be initialized to "" by the toolstack and may then be set to 0 or 1 by a guest to indicate whether it is capable of responding to the corresponding command when written to ~/control/shutdown. A toolstack may then sample the feature- value at the point of issuing a PV control command and respond accordingly:
  “0” -> the frontend should not be expected to respond, so fail the control operation “1” -> the frontend should be expected to respond, so wait for it to do so and maybe fail the control operation after some reasonable timeout. "" -> the frontend may or may not respond, so wait for it to do so and then maybe try an alternative control mechanism after some reasonable timeout.
  Since a toolstack may not initialize these paths, and the parent ~/control path is read-only to a guest, a guest should not expect a write to succeed. If it fails the guest may log the failure but should continue to process the corresponding command when written to ~/control/shutdown regardless.
*)

let xenstore_action_key = "control/shutdown"

(** actions a guest can take *)
module Action = struct
  type t =
  | PowerOff
  | Reboot
  | Suspend
  | Ignore

  let of_string = function
  | "poweroff" -> PowerOff
  | "reboot"   -> Reboot
  | "suspend"  -> Suspend
  | _          -> Ignore

  let to_string = function
  | PowerOff -> "poweroff"
  | Reboot   -> "reboot"
  | Suspend  -> "suspend"
  | Ignore   -> "ignore"
end

let features_supported =
  ["poweroff";"reboot";"suspend"]
  |> List.map (fun feat -> Printf.sprintf "control/feature-%s" feat)

(** how is a control message from the host acknowledged by the guest *)
type ack =
  | AckOK               (* ack by putting empty string *)
  | AckWrite of string  (* ack by putting string *)
  | AckNone             (* don't ack *)
  | AckDelete           (* delete key /control/shutdown *)

(** message to a guest *)
type t =
  | Now           of Action.t
  | OnShutdown    of ack * Action.t

let do_when ack action = function
  | "now"       -> Some (Now action)
  | "onshutdown"-> Some (OnShutdown (ack, action))
  | _           -> None

let ack = function
  | "ok"        -> AckOK
  | "none"      -> AckNone
  | "delete"    -> AckDelete
  | x           -> AckWrite(x)

let from_string str =
  try
    let json    = Y.from_string str in
    let ack'    = json |> U.member "ack"    |> U.to_string |> ack in
    let action' = json |> U.member "action" |> U.to_string |> Action.of_string in
      json
      |> U.member "when"
      |> U.to_string
      |> do_when ack' action'
  with
    Yojson.Json_error _ ->
      None
