/* Parser for regular expressions
Copyright (C)  2003 - 2011 Yamagata Yoriyuki
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation; either version 2 of
the License, or (at your option) any later version.

As a special exception to the GNU Library General Public License, you
may link, statically or dynamically, a "work that uses this library"
with a publicly distributed version of this library to produce an
executable file containing portions of this library, and distribute
that executable file under terms of your choice, without any of the
additional requirements listed in clause 6 of the GNU Library General
Public License. By "a publicly distributed version of this library",
we mean either the unmodified Library as distributed by the authors,
or a modified version of this library that is distributed under the
conditions defined in clause 3 of the GNU Library General Public
License. This exception does not however invalidate any other reasons
why the executable file might be covered by the GNU Library General
Public License .

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

You can contact the authour by sending email to
yoriyuki.y@gmail.com */


%{

let any =
  let excluded_chars = List.map UChar.chr_of_uint
      [0x2028; 0x2029; 0x0a; 0x0c; 0x0d; 0x85] in
  let s = List.fold_right USet.add excluded_chars USet.empty in
  USet.compl s

let line_separators =
  let cr = [UChar.chr_of_uint 0x0d] in
  let lf = [UChar.chr_of_uint 0x0a] in
  let crlf = cr @ lf in
  let ls = [UChar.chr_of_uint 0x2028] in
  let ps = [UChar.chr_of_uint 0x2029] in
  let ff = [UChar.chr_of_uint 0x0d] in
  let nel = [UChar.chr_of_uint 0x85] in
  let r = `String nel in
  let r = `Alt (`String ff, r) in
  let r = `Alt (`String lf, r) in
  let r = `Alt (`String cr, r) in
  let r = `Alt (`String crlf, r) in
  let r = `Alt (`String ps, r) in
  `Alt (`String ls, r)

let bol = `Alt (`BoS, `After line_separators)

let eol = `Alt (`EoS, `Before line_separators)

let string_of_list cs =
  let b = Buffer.create 0 in
  List.iter (Buffer.add_char b) cs;
  Buffer.contents b

let quoted_charset c =
  let s = USet.add (UChar.of_char '\\') USet.empty in
  USet.add (UChar.of_char c) s

let set_of_string s =
  let r = ref USet.empty in
  String.iter (fun c -> 
    r := USet.add (UChar.of_char c) !r)
    s;
  !r
  
%}

%token <UChar.t> UCHAR
%token <char> ASCII
%token DOT
%token ASTARISK
%token <int * (int option) * string> REPN
%token PLUS
%token QUESTION
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token MINUS
%token HAT
%token DOLLAR
%token ALT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token SPACE
%token AND
%token OR
%token COLON
%token BOS EOS
%token END

%left ALT
%left CONCAT
%left OR
%left MINUS
%left AND COLON
%left LEFT_PAREN RIGHT_PAREN
%left HAT
%left LEFT_BRACKET RIGHT_BRACKET
%left LEFT_BRACE LEFT_BRACE
%left UCHAR ASCII DOLLAR SPACE BOS EOS
%left DOT ASTARISK PLUS QUESTION REPN

%start start
%type <UReStrParserType.tree> start
%%

start : 
  regexp END {$1}
| END {`Epsilon};

regexp :
  LEFT_BRACKET charset RIGHT_BRACKET {`SetNotation $2}
| DOT {`Set any}
| HAT {bol}
| DOLLAR {eol}
| regexp ASTARISK {`Rep $1}
| regexp REPN {let n, m, _ = $2 in `Repn ($1, n, m)}
| regexp PLUS {`Seq ($1, (`Rep $1))}
| regexp QUESTION {`Alt ($1, `Epsilon)}
| regexp ALT regexp {`Alt ($1, $3)}
| regexp regexp %prec CONCAT {`Seq ($1, $2)}
| LEFT_PAREN regexp RIGHT_PAREN {`Group $2}
| uchar {`String [$1]}
| BOS {`BoS}
| EOS {`EoS}

uchar :
  UCHAR {$1}
| ASCII {UChar.of_char $1}
| MINUS {UChar.of_char '-'}
| LEFT_BRACE {UChar.of_char '{'}
| RIGHT_BRACE {UChar.of_char '}'}
| SPACE {UChar.of_char ' '}
| AND {UChar.of_char '&'}
| OR {UChar.of_char '|'}
| COLON {UChar.of_char ':'};

charset :
| head_charset_char {`Set (USet.add $1 USet.empty)}
| head_charset_char MINUS charset_char {`Set (USet.add_range $1 $3 USet.empty)}
| HAT tail_charset {`Compl $2}
| charset tail_charset %prec CONCAT {`Union ($1, $2)}
| unquote {`Set $1}
| set {$1}

tail_charset :
  charset_char {`Set (USet.add $1 USet.empty)}
| charset_char MINUS charset_char
    {`Set (USet.add_range $1 $3 USet.empty)}
| tail_charset tail_charset %prec CONCAT {`Union ($1, $2)}
| unquote {`Set $1}
| set {$1}

unquote :
| LEFT_PAREN {quoted_charset '('}
| RIGHT_PAREN {quoted_charset ')'}
| REPN {let _, _, s = $1 in set_of_string s}
| BOS {quoted_charset '`'}
| EOS {quoted_charset '\''}
| ALT {quoted_charset '|'};

set :
| LEFT_BRACE set_notation RIGHT_BRACE {$2}

common_charset_char :
  UCHAR {$1}
| ASCII {UChar.of_char $1}
| ASTARISK {UChar.of_char '*'}
| PLUS {UChar.of_char '+'}
| QUESTION {UChar.of_char '?'}
| DOT {UChar.of_char '.'}
| DOLLAR {UChar.of_char '$'}
| LEFT_BRACKET {UChar.of_char '['}
| SPACE {UChar.of_char ' '}
| AND {UChar.of_char '&'}
| OR {UChar.of_char '|'}
| COLON {UChar.of_char ':'}
| MINUS {UChar.of_char '-'};

head_charset_char :
| RIGHT_BRACKET {UChar.of_char ']'}
| common_charset_char {$1}

charset_char :
| HAT {UChar.of_char '^'}
| common_charset_char {$1}

set_notation :
  property
    {let name = string_of_list $1 in `Property name}
| LEFT_BRACKET charset RIGHT_BRACKET {$2}
| set_notation COLON set_notation {`Intr ($1, $3)}
| set_notation AND set_notation {`Intr ($1, $3)}
| set_notation OR set_notation {`Union ($1, $3)}
| set_notation MINUS set_notation {`Diff ($1, $3)}
| HAT set_notation {`Compl $2}
| set_notation SPACE {$1}
| SPACE set_notation {$2};

property :
  ASCII {[$1]}
| ASCII property {$1 :: $2};
