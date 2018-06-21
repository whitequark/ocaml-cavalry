let int_field: int Dyn.field =
  Dyn.create (fun () -> "int")
let string_field: string Dyn.field =
  Dyn.create (fun () -> "string")

let marshal x = Marshal.to_bytes x [Marshal.Closures]
let unmarshal x = Marshal.from_bytes x 0

let write n x = Marshal.to_channel (open_out n) x [Marshal.Closures]
let read n = Marshal.from_channel (open_in n)

let proj pp desc fld box =
  Format.printf " >> proj %s " desc;
  try
    Format.printf "= %a\n%!" pp (Dyn.proj fld box)
  with exn ->
    Format.printf "-> %s!\n%!" (Printexc.to_string exn)
let proj_missing desc =
  Format.printf " >> proj %s (not loaded)\n%!" desc

let fmt_int f = Format.fprintf f "%d"
let fmt_string f = Format.fprintf f "%S"
let fmt_opaque f _ = Format.fprintf f "<opaque>"

let () =
  try
    for i = 0 to int_of_string (Sys.getenv "LOAD_PLUGIN") - 1 do
      Dynlink.loadfile_private (Dynlink.adapt_filename "_build/default/dyn_test_plugin.cma")
    done
  with
  | Dynlink.Error err ->
    Format.printf "Dynlink error: %s\n" (Dynlink.error_message err)
  | Invalid_argument "int_of_string"
  | Not_found -> ()

let () =
  if Array.length Sys.argv = 1 then begin
    let int_box = Dyn.inj int_field 1 in
    let string_box = Dyn.inj string_field "a" in

    proj fmt_int    "int_field int_box"        int_field int_box;
    proj fmt_string "string_field string_box"  string_field string_box;
    proj fmt_int    "int_field string_box"     int_field (Obj.magic string_box);

    let int_box_bytes = marshal int_box in
    let string_box_bytes = marshal string_box in

    let int_box': Dyn.box = unmarshal int_box_bytes in
    let string_box': Dyn.box = unmarshal string_box_bytes in

    proj fmt_int    "int_field int_box'"       int_field int_box';
    proj fmt_string "string_field string_box'" string_field string_box';

    write "int_box.bin" int_box;
    write "string_box.bin" string_box;

    begin match !Dyn_test_common.plugin_box with
    | Some plugin_box ->
      proj fmt_int    "int_field plugin_box"        int_field plugin_box;
      proj fmt_string "string_field plugin_box"  string_field plugin_box;
      begin match !Dyn_test_common.plugin_field with
      | Some (Dyn.Any plugin_field) ->
        proj fmt_opaque "plugin_field plugin_box" plugin_field plugin_box;
      | None ->
        proj_missing "plugin_field plugin_box"
      end;

      write "plugin_box.bin" plugin_box
    | None ->
      proj_missing "int_field plugin_box";
      proj_missing "string_field plugin_box";
      proj_missing "plugin_field plugin_box"
    end
  end else begin
    let box: Dyn.box = read Sys.argv.(1) in

    proj fmt_int    "int_field box"    int_field box;
    proj fmt_string "string_field box" string_field box;
    begin match !Dyn_test_common.plugin_field with
    | Some (Dyn.Any plugin_field) ->
      proj fmt_opaque "plugin_field box" plugin_field box;
    | _ ->
      proj_missing "plugin_field box"
    end
  end
