open Mirage

let main =
  foreign
    ~packages:[package "notty"]
    "Unikernel.Make" (console @-> job)

let notty = impl @@ object
    inherit base_configurable
    method module_name = "Notty_mirage"
    method name = "notty"
    method ty = console
end


let () =
  register "rain" [main $ notty]
