let locales =
  Sys.readdir "." |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".txt")
  |> List.map Filename.chop_extension
  |> List.sort compare

let file locale = Printf.sprintf "%s.mar" locale

let rule locale =
  Printf.sprintf
    {|
(rule
 (targets %s.mar)
 (deps    (:x %s.txt) (alias database))
 (action  (run ../tools/camomilelocaledef.exe --file %%{x} .)))
|}
    locale locale

let () =
  Printf.printf
    {|

(install
 (section (site (camomile locales)))
 (files
   %s
 ))

(alias
 (name database)
 (deps (glob_files ../database/*.mar)))

%s
|}
    (String.concat "\n" (List.map file locales))
    (String.concat "\n" (List.map rule locales))
