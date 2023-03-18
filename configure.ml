#!/usr/bin/env ocaml

let () =
  let share = ref "/usr/share/camomile" in
  let args =
    [("--share", Arg.Set_string share, "DIR directory where to put data files")]
  in
  Arg.parse args ignore "Configura camomile\noptions are:";
  let oc = open_out "Camomile/installConfig.ml" in
  Printf.fprintf oc "let share_dir = %S\n" !share;
  close_out oc
