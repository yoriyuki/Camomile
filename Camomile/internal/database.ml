(* Database.ml : Unified Interfaces of Stored Data for Camomile
   Copyright (C) 2011 National Institute of Advanced Science and
   Technology
*)

let escape s =
  let b = Buffer.create 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '0'..'9' | 'a'..'z'|'A'..'Z' | '-' | '_' | '@' as c -> Buffer.add_char b c;
    | _ as c -> 
	Printf.ksprintf (Buffer.add_string b) "%%%02X" (Char.code c)
  done;
  Buffer.contents b

let read dir suffix reader key =
  let fname = escape key in
  let path = Filename.concat dir (fname ^ "." ^ suffix) in
  let c = try open_in_bin path with Sys_error _  -> raise Not_found in
  let v = reader c in
  close_in c;
  v

let write dir suffix writer key data =
  let fname = escape key in
  let path = Filename.concat dir (fname ^ "." ^ suffix) in
  let c = try open_out_bin path with Sys_error _  -> raise Not_found in
  writer c data;
  close_out c;

