(* This is a new script to de-entangle from cyclic dependencies in
   [parse_scripts.ml] *)

let range_pat =
  Str.regexp
    "\\([0-9A-Fa-f]+\\)\\.\\.\\([0-9A-Fa-f]+\\)[ \t]*;[ \t]*\\([^ \t]+\\)"

let num_pat = Str.regexp "\\([0-9A-Za-z]+\\)+[ \t]*;[ \t]*\\([^ \t]+\\)"

let gen_script_type () =
  let ic = open_in "unidata/Scripts.txt" in
  let rec f names =
    try
      let s = input_line ic in
      if Str.string_match range_pat s 0 then f (Str.matched_group 3 s :: names)
      else if Str.string_match num_pat s 0 then
        f (Str.matched_group 2 s :: names)
      else f names
    with End_of_file ->
      close_in ic;
      names
  in
  let names = List.sort_uniq Stdlib.compare (f []) in
  Printf.printf "type t = [\n";
  List.iter
    (fun name -> Printf.printf "  | `%s\n" (String.capitalize_ascii name))
    names;
  Printf.printf "]\n\n";

  Printf.printf "let name_of_script_type = function\n";
  List.iter
    (fun name ->
      Printf.printf "  | `%s -> %S\n"
        (String.capitalize_ascii name)
        (String.lowercase_ascii name))
    names;
  Printf.printf "\n\n";

  Printf.printf
    "let script_type_of_name name = match String.lowercase_ascii name with\n";
  List.iter
    (fun name ->
      Printf.printf "  | %S -> `%s\n"
        (String.lowercase_ascii name)
        (String.capitalize_ascii name))
    names;
  Printf.printf "  | _ -> raise Not_found\n\n";

  Printf.printf "let num_of_script = function\n";
  List.iteri
    (fun pos name ->
      Printf.printf "  | `%s -> %d\n" (String.capitalize_ascii name) pos)
    names;
  Printf.printf "\n\n";

  Printf.printf "let script_of_num = function\n";
  List.iteri
    (fun pos name ->
      Printf.printf "  | %d -> `%s\n" pos (String.capitalize_ascii name))
    names;
  Printf.printf "  | _ -> raise Not_found\n\n"

let () =
  match Sys.argv with
    | [| _; "--gen-script-type" |] -> gen_script_type ()
    | _ -> failwith "invalid command line"
