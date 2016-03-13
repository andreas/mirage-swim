open Mirage

let join_ip =
  let doc = Key.Arg.info ~doc:"IP of other node to join." ["join_ip"] in
  Key.(create "join_ip" Arg.(opt string "" doc))

let port =
  let doc = Key.Arg.info ~doc:"Port for serving requests." ["port"] in
  Key.(create "port" Arg.(opt int 53 doc))

let tracing = mprof_trace ~size:1000000 ()

let handler =
  foreign
    ~keys:[
      Key.abstract join_ip;
      Key.abstract port
    ]
    "Unikernel.Main"
    (console @-> stackv4 @-> time @-> random @-> job)

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | "socket" -> `Socket
    | _ -> `Direct
  with Not_found -> `Direct

let dhcp =
  try match Sys.getenv "DHCP" with
    | "" -> false
    | _ -> true
  with Not_found -> false

let stack =
  match net, dhcp with
  | `Direct, true -> direct_stackv4_with_dhcp default_console tap0
  | `Direct, false -> direct_stackv4_with_default_ipv4 default_console tap0
  | `Socket, _ -> socket_stackv4 default_console [Ipaddr.V4.any]

let () =
  add_to_opam_packages ["res"; "core_kernel"; "bin_prot"; "sexplib"];
  add_to_ocamlfind_libraries ["res"; "core_kernel"; "bin_prot"; "bin_prot.syntax"; "sexplib"; "sexplib.syntax"];
  register "swim" ~tracing [
    handler $ default_console $ stack $ default_time $ default_random;
  ]

