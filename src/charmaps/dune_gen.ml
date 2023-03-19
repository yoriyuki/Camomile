open StdLabels

module Charmap_decode = struct
  let extract_words s ~is_word_char =
    let open String in
    let rec skip_blanks i =
      if i = length s then []
      else if is_word_char s.[i] then parse_word i (i + 1)
      else skip_blanks (i + 1)
    and parse_word i j =
      if j = length s then [sub s ~pos:i ~len:(j - i)]
      else if is_word_char s.[j] then parse_word i (j + 1)
      else sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
    in
    skip_blanks 0

  let extract_blank_separated_words s =
    extract_words s ~is_word_char:(function ' ' | '\t' -> false | _ -> true)

  exception Break

  let parse_header file =
    let inchan = open_in file in
    let codeset_name = ref (Filename.basename file) in
    let aliases = ref [] in
    let escape_char = ref '/' in
    let comment_char = ref '#' in
    try
      while true do
        let s = input_line inchan in
        match extract_blank_separated_words s with
          | ["<code_set_name>"; s] -> codeset_name := s
          | ["<comment_char>"; s] -> comment_char := s.[0]
          | ["<escape_char>"; s] -> escape_char := s.[0]
          | "<mb_cur_min>" :: _ | "<mb_cur_max>" :: _ -> ()
          | "%" :: "alias" :: a | "%alias" :: a -> aliases := a @ !aliases
          | [] -> ()
          | s :: _ when s.[0] = !comment_char -> ()
          | "CHARMAP" :: _ -> raise Break
          | _ -> raise Break
      done;
      assert false
    with Break ->
      close_in inchan;
      (!codeset_name, !aliases)
end

let escape s =
  let b = Buffer.create 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      | ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' | '@') as c ->
          Buffer.add_char b c
      | _ as c -> Printf.ksprintf (Buffer.add_string b) "%%%02X" (Char.code c)
  done;
  Buffer.contents b

let () =
  let sources = Sys.argv.(1) in
  let charmaps =
    Sys.readdir sources |> Array.to_list
    |> List.sort ~cmp:String.compare
    |> List.map ~f:(fun fn ->
           let path = Filename.concat sources fn in
           let codeset, aliases = Charmap_decode.parse_header path in
           let targets =
             (escape codeset ^ ".mar")
             :: List.map ~f:(fun s -> escape s ^ ".mar") aliases
           in
           (fn, targets))
  in
  let to_install = List.map charmaps ~f:snd |> List.concat in
  let buf = Buffer.create 10_000 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  pr "";
  pr "(install";
  pr " (section (site (camomile charmaps)))";
  pr " (files";
  List.iter to_install ~f:(fun fn -> pr "   %s" fn);
  pr "    ))";
  List.iter charmaps ~f:(fun (fn, targets) ->
      pr "";
      pr "(rule";
      pr " (targets %s)" (String.concat ~sep:" " targets);
      pr " (deps    %s)" fn;
      pr " (action  (run ../tools/camomilecharmap.exe -d . %%{deps})))");
  print_endline (Buffer.contents buf)
