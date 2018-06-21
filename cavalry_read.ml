let () =
  let chan = open_in_bin Sys.argv.(1) in
  let file = Cavalry.input_file chan in
  Format.printf "%a\n" Cavalry.pp_file file
