open Mirage

(* Use `FS` to set the underlying filesystem:
   FS=crunch (or nothing): use static filesystem via crunch
   FS=fat: use FAT and block device (run ./make-fat-images.sh)
 *)
let mode =
  try match String.lowercase (Unix.getenv "FS") with
    | "fat"   -> `Fat
    | "irmin" -> `Irmin
    | _       -> `Crunch
  with Not_found ->
    `Crunch

let uri = match mode with
  | `Fat | `Crunch -> ""
  | `Irmin ->
    try Unix.getenv "URI" with Not_found ->
      Printf.eprintf "The Irmin FS backend needs URI to be set.\n%!";
      exit 1

let path = match mode with
  | `Fat | `Crunch -> None
  | `Irmin -> try Some (Unix.getenv "FSROOT") with Not_found -> None

let fat_ro dir =
  kv_ro_of_fs (fat_of_files ~dir ())

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | "socket" -> `Socket
    | _        -> `Direct
  with Not_found -> `Direct

let dhcp =
  try match Sys.getenv "DHCP" with
    | "" -> false
    | _  -> true
  with Not_found -> false

let console0 = default_console

let stack =
  match net, dhcp with
  | `Direct, true  -> direct_stackv4_with_dhcp console0 tap0
  | `Direct, false -> direct_stackv4_with_default_ipv4 console0 tap0
  | `Socket, _     -> socket_stackv4 console0 [Ipaddr.V4.any]

let fs = match mode with
  | `Fat    -> fat_ro "./htdocs"
  | `Crunch -> crunch "./htdocs"
  | `Irmin  -> irmin stack ?path uri

let http_srv = http_server (conduit_direct ~tls:true stack)

let main =
  foreign "Dispatch.Main" (console @-> kv_ro @-> http @-> job)

let () =
  add_to_ocamlfind_libraries ["re.str"];
  add_to_opam_packages ["re"];

  register "www" [
    main $ default_console $ fs $ http_srv
  ]
