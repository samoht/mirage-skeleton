
let () = Random.self_init ()

let rec (--) a b = if a > b then [] else a :: succ a -- b

let utf8_of_code_point =
  let buf = Buffer.create 7 in fun cp ->
    Buffer.clear buf;
    Uutf.Buffer.add_utf_8 buf (Uchar.of_int cp);
    Buffer.contents buf

let nsym = 4096
let glitch = nsym / 20
let symbols = Array.(concat [
  init 58 (fun x -> utf8_of_code_point (0xff66 + x));
  init 10 (fun x -> utf8_of_code_point (0x30 + x));
  (* init 26 (fun x -> utf8_of_code_point (0x61 + x)); *)
  (* init 14 (fun x -> utf8_of_code_point (0x21 + x)); *)
])
let sym () = symbols.(Random.int (Array.length symbols))
let syms = Array.init nsym (fun _ -> sym ())

let gen_wait h = `Wait Random.(int (h / 2))
and gen_line h =
  `Line Random.(0, int (nsym - h), int (h + h / 2) + 1, int 2 + 1)
let gen (w, h as dim) =
  let lines = 1 -- w |> List.map @@ fun _ ->
    if Random.float 1. < 0.1 then gen_line h else gen_wait h in
  (dim, lines)

let step ((_, h as dim), xs) =
  let xs = xs |> List.map @@ function
      `Wait 0 -> gen_line h
    | `Wait n -> `Wait (n - 1)
    | `Line (i, _, win, k) when i - win + k >= h -> gen_wait h
    | `Line (i, s, win, k) -> `Line (i + k, s, win, k) in
  Random.(for _ = 0 to int glitch do syms.(int nsym) <- sym () done);
  (dim, xs)

open Notty
open Notty.Infix

let bgc = A.(bg @@ rgb ~r:0 ~g:0 ~b:0)

let color i n =
  let chan x = x *. 255. |> truncate
  and t  = float i /. float n in
  let t1 = exp (-. t /. 0.02) |> chan
  and t2 = exp (-. t /. 0.45) |> chan in
  A.rgb_888 ~r:t1 ~b:t1 ~g:t2

let show ((w, h), xs) =
  let f = function
    `Wait _ -> I.void 1 0
  | `Line (i, sym, win, _) ->
      let last = i - win
      and off = max 0 (i - h + 1) in
      let rec chars w =
        let ix = w + last in
        if 0 <= min ix w then syms.(sym + ix) :: chars (w - 1) else [] in
      let rec images acc i = function
        | []    -> acc
        | x::xs -> let img = I.string A.(fg (color i win) ++ bgc) x in
                   images (img :: acc) (i + 1) xs in
      chars (win - off) |> images [] off
        |> I.vcat |> I.vpad (max 0 (i - win)) 0 in
  (List.map f xs |> I.hcat) </> I.char bgc ' ' w h

open Notty_mirage
module Make (C: Mirage_console_lwt.S) = struct

  open Lwt.Infix
  module T = Terminal_link_of_console(C)

  type t = {
    output: C.t;
    trm   : Tmachine.t
  ; buf   : Buffer.t
  }

  let create c = {
    output = c;
    trm    = Tmachine.create ~mouse:false ~bpaste:false Cap.ansi;
    buf    = Buffer.create 4096;
  }
  let write t =
    Buffer.clear t.buf;
    Tmachine.output t.trm t.buf;
    let buf = Cstruct.of_string (Buffer.contents t.buf) in
    C.write t.output buf

  let release t =if Tmachine.release t.trm then write t else Lwt.return (Ok ())

  let image t image  = Tmachine.image t.trm image; write t

  type r = [ Unescape.event | `Resize of int * int | `End | `Timer ]

  (* XXX: use delay *)

  let loop t st =
    let rec go st =
      image t (show st) >>= function
      | Ok () -> go (step st)
      | _     -> Lwt.return ()
    in
    go st

  let start c =
    let t = create c in
    loop t (gen (80, 80)) >>= fun () ->
    print_endline "XXX\n";
    release t >|= function
    | Ok ()   -> ()
    | Error _ -> ()

end

module X = Make(Console_unix)

open Lwt.Infix

let () =
  Lwt_main.run begin
    Console_unix.connect "0" >>= fun t ->
    X.start t
  end
