val xenstore_action_key : string

val features_supported : string list

(** actions a guest can take *)
module Action : sig
  type t =
  | PowerOff
  | Reboot
  | Suspend
  | Ignore

  val of_string : string -> t

  val to_string : t -> string
end

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

(** [from_string str] reads a JSON object [str] and returns a [t]
    value that represents it *)
val from_string: string -> t option
