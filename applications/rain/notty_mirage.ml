open Lwt.Infix

(* XXX General, common up, possibly Mirage-Types. *)
module type SFLOW = sig

  type +'a io

  type input
  type output
  type flow
  type error
  type write_error
  val pp_error: error Fmt.t
  val pp_write_error: write_error Fmt.t
  val read   : flow -> ([`Data of input | `Eof], error) result io
  val write  : flow -> output -> (unit, write_error) result io
  val writev : flow -> output list -> (unit, write_error) result io
  val close  : flow -> unit io

end

module type SFLOW_LWT = SFLOW with type 'a io = 'a Lwt.t
(* /XXX *)

(* Seriously? It's 2016 and this isn't in the standard library?? *)
let rec fmap f = function
  | []    -> []
  | x::xs -> match f x with Some y -> y :: fmap f xs | _ -> fmap f xs

let ok x = Lwt.return (Ok x)

module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

module Terminal_link_of_console (C : Mirage_console_lwt.S) = struct
  type 'a io = 'a Lwt.t
  type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
  type output = [ `Data of Cstruct.t | `Line_edit of bool ]
  type flow  = C.t
  type error = C.error
  type write_error = C.write_error
  let pp_error = C.pp_error
  let pp_write_error = C.pp_write_error

  let close = C.close
  let writev t xs =
    fmap (function `Data x -> Some x | _ -> None) xs |> C.writev t
  let write t = function
    | `Data buf -> C.write t buf
    | _         -> ok ()
  let read t = C.read t >|= function
    | Ok (`Data buf) -> Ok (`Data (`Data buf))
    | Ok `Eof        -> Ok `Eof
    | Error _ as e   -> e
end
