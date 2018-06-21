type direction = Left | Right
let direction_field: direction Dyn.field =
  Dyn.create (fun () -> "direction")

let () =
  Dyn_test_common.plugin_field := Some (Dyn.Any direction_field);
  Dyn_test_common.plugin_box := Some (Dyn.inj direction_field Left);

