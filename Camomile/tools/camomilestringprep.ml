(* Copyright 2010 Pierre Chambart *)

open StringPrep_data

let hashcons_list =
  let tbl = Hashtbl.create 10 in
  let rec f = function
    | [] -> []
    | h::q as l ->
	try
	  Hashtbl.find tbl l
	with
	  | Not_found ->
	      let q = f q in
	      let l = h::q in
	      Hashtbl.add tbl l l;
	      l
  in
  f

let hashcons_mapping =
  let tbl = Hashtbl.create 10 in
  let f x =
    try
      Hashtbl.find tbl x
    with
      | Not_found ->
	  match x with
	    | Diff v ->
		Hashtbl.add tbl x x;
		x
	    | List l ->
		let x = List (hashcons_list l) in
		Hashtbl.add tbl x x;
		x
  in
  f

let input_dir = ref ""
let output_dir = ref ""

let () = Arg.parse ["-in",Arg.Set_string input_dir,"input directory";
		    "-out",Arg.Set_string output_dir,"output directory"]
  (fun s -> ()) "Parse stringprep data file";

module MappingHash =
struct
  type t = mapping
  let hash = Hashtbl.hash
  let equal = (=)
end

module MappingMap = UCharTbl.Make ( MappingHash )

let mapping_of_list index = function
  | [value] -> Diff ((UChar.code value) - (UChar.code index))
  | l -> List l

let umap_of_list l =
  let f map (index,l) =
    let mapping = hashcons_mapping (mapping_of_list index l) in
    UMap.add ( index) mapping map
  in
  List.fold_left f UMap.empty l

let uset_of_list l =
  let f s (x,y) = USet.add_range (UChar.of_int x) (UChar.of_int y) s in
  List.fold_left f USet.empty l

let umap_union m1 m2 =
  UMap.fold_range UMap.add_range m1 m2

let char_of_string s =
  UChar.chr_of_uint (int_of_string ("0x"^s))

let pat_range =
  Str.regexp "[ ]*\\([0-9A-Fa-f]+\\)-\\([0-9A-Fa-f]+\\)"
let pat_single =
  Str.regexp "[ ]*\\([0-9A-Fa-f]+\\)"

let parse_set_line set s =
  if Str.string_match pat_range s 0
  then
    let u1 = char_of_string (Str.matched_group 1 s) in
    let u2 = char_of_string (Str.matched_group 2 s) in
    USet.add_range u1 u2 set
  else
    let _ = Str.string_match pat_single s 0 in
    let u = char_of_string (Str.matched_group 1 s) in
    USet.add u set

exception Ok of string

let parse_set file =
  let c = open_in file in
  let rec parse set =
    try
      raise (Ok (input_line c))
    with
      | End_of_file -> set
      | Ok s ->
	  let set = parse_set_line set s in
	  parse set
  in
  parse USet.empty

let pat_name =
  Str.regexp "[ ]*\\([0-9A-Fa-f]+\\);"
let pat_end =
  Str.regexp "[ ]*;"
let pat_letter =
  Str.regexp " \\([0-9A-Fa-f]+\\)"

let parse_map_line s =
  let _ = Str.string_match pat_name s 0 in
  let name = char_of_string (Str.matched_group 1 s) in
  let pos = Str.match_end () in
  let rec f pos l =
    if Str.string_match pat_end s pos
    then List.rev l
    else
      let _ = Str.string_match pat_letter s pos in
      let c = char_of_string (Str.matched_group 1 s) in
      let pos = Str.match_end () in
      f pos (c::l)
  in
  name,f pos []

let parse_map file =
  let c = open_in file in
  let rec parse l =
    try
      raise (Ok (input_line c))
    with
      | End_of_file -> l
      | Ok s ->
	  let line = parse_map_line s in
	  parse (line::l)
  in
  parse []

let a1 = parse_set (Filename.concat !input_dir "/a1")
let c11 = parse_set (Filename.concat !input_dir "/c11")
let c12 = parse_set (Filename.concat !input_dir "/c12")
let c21 = parse_set (Filename.concat !input_dir "/c21")
let c22 = parse_set (Filename.concat !input_dir "/c22")
let c3 = parse_set (Filename.concat !input_dir "/c3")
let c4 = parse_set (Filename.concat !input_dir "/c4")
let c5 = parse_set (Filename.concat !input_dir "/c5")
let c6 = parse_set (Filename.concat !input_dir "/c6")
let c7 = parse_set (Filename.concat !input_dir "/c7")
let c8 = parse_set (Filename.concat !input_dir "/c8")
let c9 = parse_set (Filename.concat !input_dir "/c9")
let d1 = parse_set (Filename.concat !input_dir "/d1")
let d2 = parse_set (Filename.concat !input_dir "/d2")

let b1_list = parse_map (Filename.concat !input_dir "/b1")
let b2_list = parse_map (Filename.concat !input_dir "/b2")
let b3_list = parse_map (Filename.concat !input_dir "/b3")

let b1 = umap_of_list b1_list
let b2 = umap_of_list b2_list
let b3 = umap_of_list b3_list

let nodeprep_prohibited_list =
  [ 0x0022;
    0x0026;
    0x0027;
    0x002F;
    0x003A;
    0x003C;
    0x003E;
    0x0040; ]
let nodeprep_prohibited_set = uset_of_list ( List.map ( fun x -> x,x ) nodeprep_prohibited_list )

let saslprep_map =
  let f c map =
    UMap.add c (mapping_of_list c [ UChar.of_int 0x0020 ]) map
  in
  USet.fold f c12 UMap.empty
(** Non-ASCII space characters mapped to 0x0020 ( RFC 4013 ) *)

let iscsi_prohibited_list =
  [ 0x3002,0x3002;
    0x0000,0x002C;
    0x002F,0x002F;
    0x003B,0x0040;
    0x005B,0x0060;
    0x007B,0x007F; ]
let iscsi_prohibited_set = uset_of_list iscsi_prohibited_list

let make_map l =
  let map = List.fold_left umap_union UMap.empty l in
  MappingMap.of_map (Diff 0) map

let make_set l =
  let set = List.fold_left USet.union USet.empty l in
  UCharTbl.Bool.of_set set

let map_b1b2 = make_map [b1;b2]
let map_b1 = make_map [b1]
let saslprep_map = make_map [b1;saslprep_map]
let nodeprep_prohibited = make_set [a1;c11;c12;c21;c22;c3;c4;c5;c6;c7;c8;c9;nodeprep_prohibited_set]
let resourceprep_prohibited = make_set [a1;c12;c21;c22;c3;c4;c5;c6;c7;c8;c9]
let nameprep_prohibited = make_set [a1;c12;c22;c3;c4;c5;c6;c7;c8;c9]
let saslprep_prohibited = make_set [a1;c12;c21;c22;c3;c4;c5;c6;c7;c8;c9]
let trace_prohibited = make_set [c21;c22;c3;c4;c5;c6;c8;c9]
let iscsi_prohibited = make_set [a1;c11;c12;c21;c22;c3;c4;c5;c6;c7;c8;c9;iscsi_prohibited_set]
let mib_prohibited = make_set [a1;c21;c22;c3;c4;c5;c6;c7;c8;c9]
let d1_table = make_set [d1]
let d2_table = make_set [d2]

let write name value = Database.write !output_dir "mar" output_value name value;;

write "map_b1b2" map_b1b2;
write "map_b1" map_b1;
write "saslprep_map" saslprep_map;
write "nodeprep_prohibited" nodeprep_prohibited;
write "resourceprep_prohibited" resourceprep_prohibited;
write "nameprep_prohibited" nameprep_prohibited;
write "saslprep_prohibited" saslprep_prohibited;
write "trace_prohibited" trace_prohibited;
write "iscsi_prohibited" iscsi_prohibited;
write "mib_prohibited" mib_prohibited;
write "d1" d1_table;
write "d2" d2_table;

