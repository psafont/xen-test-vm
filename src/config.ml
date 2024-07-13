open Mirage

let packages =
  [
    package "logs";
    package "lwt";
    package "mirage-console-xen";
    package "mirage-logs";
    package "mirage-xen";
    package ~min:"4.6.0" "mirage-runtime";
    package "yojson";
    package "xenstore";
  ]

let main =
  main ~packages "Unikernel.Main" (time @-> job)

let () = register "xenserver-test-vm" [ main $ default_time ]
