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

(*
 * Ideally, someone working on, say, _telnet_, would map this space out and we
 * would model the input/output types after that.
 *)
module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

module Terminal_link_of_console (C : Mirage_console_lwt.S) :
  TERMINAL_LINK with type flow = C.t
