/* $Id: colParser.mly,v 1.12 2006/08/13 17:27:30 yori Exp $
   Copyright 2003 Yamagata Yoriyuki */

%{
open Toolslib
open UCharInfo
open UCol
open AbsCe


let parse_error _ = failwith "Syntax error"
let acset : aceset_info = Unidata.read_data "acset"

    %}

  %token <UChar.t> UCHAR
  %token <string list> OPTION
  %token PRIMARY SECONDARY TERTIARY EQ RESET EXPAND PREFIX EOF
  %nonassoc RESET
  %left PRIMARY SECONDARY TERTIARY EQ
  %left EXPAND 
  %right PREFIX

  %start main
  %type <AbsCe.ace_info> main
  %%
main : 
  header rules EOF
  {let ceset = acset.lowercase_first_tbl in
  let ace_info = create_ace_info ceset in
  let ace_info = $1 ace_info in
  {ace_info with ceset = $2 ace_info.ceset}} 
| rules EOF
  {let ceset = acset.lowercase_first_tbl in
  let ace_info = create_ace_info ceset in
  {ace_info with ceset = $1 ace_info.ceset}} 
| header EOF
    {let ceset = acset.lowercase_first_tbl in
    $1 (create_ace_info ceset)}
| EOF
    {create_ace_info acset.lowercase_first_tbl};
  
  header : 
    header_option header 
    {fun env ->
      $2 ($1 env)}
| header_option 
    {fun env -> $1 env};

  header_option :
    OPTION {fun env ->
      match $1 with
	["alternate"; "non-ignorable"] ->
	  {env with variable_option = `Non_ignorable}
      | ["alternate"; "shifted"] ->
	  {env with variable_option = `Shifted}
      | ["alternate"; "shifted-trimmed"] ->
	  {env with variable_option = `Shift_Trimmed}
      | ["alternate"; "blanked"] ->
	  {env with variable_option = `Blanked}
      | ["backwards"; "2"] -> 
	  {env with french = true}
      | ["backwards"; x] ->
	  failwith 
	    ("backward comparison for the level " ^ x ^ " is not supported.")
      | ["normalization"; _] ->
	  prerr_endline "Warning : normalization option is not supported";
	  env
      | ["caseLevel"; _] ->
	  prerr_endline "Warning : caseLevel option is not supported";
	  env

      | [("caseFirst" | "casefirst"); ("off" | "lower")] ->
	  let ceset = acset.lowercase_first_tbl in
	  {env with ceset = ceset}
      | [("caseFirst" | "casefirst"); "upper"] ->
	  let ceset = acset.uppercase_first_tbl in
	  {env with ceset = ceset}
      | ["strength"; _] ->
	  prerr_endline "Warning : strength option is not supported";
	  env
      | ["hiraganaQ"; _] ->
	  let ceset = env.ceset in
	  let ce, ceset = add_after `Primary (last_variable ceset) ceset in
	  let ceset = put `HiraganaQ [ce] ceset in
	  {env with ceset = ceset; hiraganaQ = true}
      | x -> 
	  let s = String.concat " " x in
	  failwith ("unknown option:" ^ s)}
  | UCHAR {
    fun env ->
      if $1 = UChar.of_char '@' then
	{env with french = true}
      else
	failwith "stray character"};

  rules : 
    rule rules 
    {fun env -> 
      let _, _, env = $1 env in
      $2 env}
| rule {fun env -> let _, _, env = $1 env in env};

  rule :
  RESET init {$2}
| RESET OPTION init {
  fun ceset ->    
    match $2 with
      ["before"; depth] ->
	let prec =
	  match int_of_string depth with
	    1 -> `Primary
	  | 2 -> `Secondary
	  | 3 -> `Tertiary
	  | _ -> failwith ("Level " ^ depth ^ " is not supported")
	in
	let pos, exp, ceset = $3 ceset in
	(prev prec pos ceset, exp, ceset)
    | _ -> failwith "Unknown option"}
| rule PRIMARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = add_after `Primary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule SECONDARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = add_after `Secondary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule TERTIARY elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let pos', ceset = add_after `Tertiary pos ceset in
    let ceset' = $3 [] (pos' :: exp) ceset in
    (pos', exp, ceset')}
| rule EQ elem {
  fun ceset ->
    let pos, exp, ceset = $1 ceset in
    let ceset' = $3 [] (pos :: exp) ceset in
    (pos, exp, ceset')};

  ulist : UCHAR {[$1]} | UCHAR ulist {$1 :: $2};

  init :
    ulist {
  fun ceset ->
    let ceset, es = ces_of ceset $1 in
    (List.hd es, List.tl es, ceset)}
| OPTION {
  fun ceset ->
    match $1 with
      [("first" | "last") ; "teritiary"; "ignorable"]	
    | [("first" | "last") ; "secondary"; "ignorable"] ->
	(complete_ignorable ceset, [], ceset)
    | ["first"; "primary"; "ignorable"] ->
	let ce = next `Secondary (complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "primary"; "ignorable"] ->
	let ce = next `Primary (complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["first"; "variable"] ->
	let ce = next `Primary (complete_ignorable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "variable"] ->
	(last_variable ceset, [], ceset)
    | ["first"; "regular"] ->
	let ce = next `Primary (last_variable ceset) ceset in
	(ce, [], ceset)
    | ["last"; "regular"] | ["top"] -> 
	let ce = prev `Tertiary (first_implicit ceset) ceset in
	(ce, [], ceset)
    | ["first"; "implicit"] ->
	(first_implicit ceset, [], ceset)
    | ["last"; "implicit"] ->
	let ce = prev `Tertiary (first_trailing ceset) ceset in
	(ce, [], ceset)
    | ["first"; "trailing"] ->
	(first_trailing ceset, [], ceset)
    | ["last"; "trailing"] ->
	(top ceset, [], ceset)
    | _ -> assert false};

  elem :
    ulist {fun prefix ces ceset ->
      put (`Seq (prefix @ $1)) ces ceset}
| elem EXPAND ulist {fun prefix ces ceset ->
    let ceset', exps = ces_of ceset $3 in
    $1 prefix (ces @ exps) ceset'}
| ulist PREFIX elem {fun prefix ces ceset ->
    let ceset', exps = ces_of ceset $1 in
    $3 (prefix @ $1) (exps @ ces) ceset'}
| OPTION {fun prefix ces ceset ->
    match $1 with ["variable"; "top"] -> 
      (match prefix, ces with
	[], [pos] ->
	  put `LastVariable ces ceset
      | _, _ ->
	  failwith "Variable top should be neither contraction nor prefixed.")
    | _ -> failwith "Unknown option"};
