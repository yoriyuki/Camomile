(* Copyright (C) 2018 Yamagata Yoriyuki.*)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yoriyuki.y@gmail.com *)

let ucd_or_die inf = try
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in
  match Uucd.decode d with
  | `Ok db -> db
  | `Error e ->
    let (l0, c0), (l1, c1) = Uucd.decoded_range d in
    Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
    exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let convert_boolean_prop_to_tbl repertoire uucd_prop =
  let cpmap = Uucd.Cpmap.map (fun props ->
      Uucd.find props uucd_prop) repertoire in
  let set = Uucd.Cpmap.fold (fun cp p set ->
      match p with
        Some true -> USet.add (UChar.of_int cp) set
      | _ -> set) cpmap USet.empty in
  Unidata.Boolean (UCharTbl.Bool.of_set set)

module type Property = sig type t end

module GenVariantTbl(P: Property) = struct

  module Comparable(P : Property) = struct
    type t = P.t
    let compare e1 e2 = Pervasives.compare e1 e2
  end

  module PSet = Set.Make(Comparable(P))
  module PMap = Map.Make(Comparable(P))

  let convert_prop_to_tbl repertoire uucd_prop =
    let cpmap = Uucd.Cpmap.map (fun props ->
        Uucd.find props uucd_prop) repertoire in
    let prop_map = Uucd.Cpmap.fold (fun cp p m ->
        UMap.add (UChar.of_int cp) p m) cpmap UMap.empty in
    let unoption = UMap.fold_range (fun first last v unoption ->
        match v with
          None -> unoption
        | Some real_value -> UMap.add_range first last real_value unoption)
        prop_map UMap.empty in
    let val_set = UMap.fold_range (fun _ _ v val_set ->
        PSet.add v val_set) unoption PSet.empty in
    if PSet.cardinal val_set > 254 then
      failwith "Number of propoty values exceeds 254"
    else
      let id_to_prop = Array.of_list (PSet.elements val_set) in
      let prop_to_id =
        let ref_pmap = ref PMap.empty in
        for i = 0 to Array.length(id_to_prop) do
          ref_pmap := PMap.add id_to_prop.(i) (i+1) !ref_pmap
        done;
        !ref_pmap in
      let propid_map = UMap.map (fun p -> PMap.find p prop_to_id) unoption in
      Unidata.Variant (id_to_prop, UCharTbl.Bits.of_map 0 propid_map)
end

let convert_byte_prop_to_tbl repertoire uucd_prop =
  let cpmap = Uucd.Cpmap.map (fun props ->
      Uucd.find props uucd_prop) repertoire in
  let m = Uucd.Cpmap.fold (fun cp p m ->
      match p with
      Some n ->
        if n > 255 then
          failwith "Byte property is > 255"
        else
          UMap.add (UChar.of_int cp) (Char.chr n) m
      |None -> m) cpmap UMap.empty in
  Unidata.Byte (UCharTbl.Char.of_map '\000' m)

module GenTbl(P: Property) = struct
  module Option(P : Property) = struct
    type t = P.t option
    let equal e1 e2 = Pervasives.compare e1 e2 == 0
    let hash = Hashtbl.hash
  end

  module UTbl = UCharTbl.Make(Option(P))

  let convert_prop_to_map repertoire uucd_prop =
    let cpmap = Uucd.Cpmap.map (fun props ->
        Uucd.find props uucd_prop) repertoire in
    let m = Uucd.Cpmap.fold (fun cp p m ->
        UMap.add (UChar.of_int cp) p m) cpmap UMap.empty in
    m

  let convert_prop_to_tbl repertoire uucd_prop =
    let m = convert_prop_to_map repertoire uucd_prop in
    Unidata.Any (UTbl.of_map None m)
end

let uucd_boolprops = [
  (Uucd.alphabetic, "alphabetic");
  (Uucd.ascii_hex_digit, "ascii_hex_digit");
  (Uucd.bidi_control, "bidi_control");
  (Uucd.bidi_mirrored, "bidi_mirrored");
  (Uucd.case_ignorable, "case_ignorable");
  (Uucd.cased, "cased");
  (Uucd.changes_when_casefolded, "changes_when_casefolded");
  (Uucd.changes_when_casemapped, "changes_when_casemapped");
  (Uucd.changes_when_lowercased, "changes_when_lowercased");
  (Uucd.changes_when_nfkc_casefolded, "changes_when_nfkc_casefolded");
  (Uucd.changes_when_titlecased, "changes_when_titlecased");
  (Uucd.changes_when_uppercased, "changes_when_uppercased");
  (Uucd.composition_exclusion, "composition_exclusion");
  (Uucd.dash, "dash");
  (Uucd.default_ignorable_code_point, "default_ignorable_code_point");
  (Uucd.deprecated, "deprecated");
  (Uucd.diacritic, "diacritic");
  (Uucd.expands_on_nfc, "expands_on_nfc");
  (Uucd.expands_on_nfd, "expands_on_nfd");
  (Uucd.expands_on_nfkc, "expands_on_nfkc");
  (Uucd.expands_on_nfkd, "expands_on_nfkd");
  (Uucd.extender, "extender");
  (Uucd.full_composition_exclusion, "full_composition_exclusion");
  (Uucd.grapheme_base, "grapheme_base");
  (Uucd.grapheme_extend, "grapheme_extend");
  (Uucd.grapheme_link, "grapheme_link");
  (Uucd.hex_digit, "hex_digit");
  (Uucd.hyphen, "hyphen");
  (Uucd.id_continue, "id_continue");
  (Uucd.id_start, "id_start");
  (Uucd.ideographic, "ideographic");
  (Uucd.ids_binary_operator, "ids_binary_operator");
  (Uucd.ids_trinary_operator, "ids_trinary_operator");
  (Uucd.join_control, "join_control");
  (Uucd.logical_order_exception, "logical_order_exception");
  (Uucd.lowercase, "lowercase");
  (Uucd.math, "math");
  (Uucd.noncharacter_code_point, "noncharacter_code_point");
  (Uucd.other_alphabetic, "other_alphabetic");
  (Uucd.other_default_ignorable_code_point, "other_default_ignorable_code_point");
  (Uucd.other_grapheme_extend, "other_grapheme_extend");
  (Uucd.other_id_continue, "other_id_continue");
  (Uucd.other_id_start, "other_id_start");
  (Uucd.other_lowercase, "other_lowercase");
  (Uucd.other_math, "other_math");
  (Uucd.other_uppercase, "other_uppercase");
  (Uucd.pattern_syntax, "pattern_syntax");
  (Uucd.pattern_white_space, "pattern_white_space");
  (Uucd.prepended_concatenation_mark, "prepended_concatenation_mark");
  (Uucd.quotation_mark, "quotation_mark");
  (Uucd.radical, "radical");
  (Uucd.regional_indicator, "regional_indicator");
  (Uucd.soft_dotted, "soft_dotted");
  (Uucd.sterm, "sterm");
  (Uucd.terminal_punctuation, "terminal_punctuation");
  (Uucd.unified_ideograph, "unified_ideograph");
  (Uucd.uppercase, "uppercase");
  (Uucd.variation_selector, "variation_selector");
  (Uucd.white_space, "white_space");
  (Uucd.xid_continue, "xid_continue");
  (Uucd.xid_start, "xid_start");
]

let () =
  let dir = ref "." in
  let ucd_file = ref "ucd.all.grouped.xml" in
  let argspecs = [("-output-dir", Arg.Set_string dir, "output directory")] in

  Arg.parse argspecs (fun fname -> ucd_file := fname)
    "Parsing UCD XML file and generate database";

  let ucd = ucd_or_die !ucd_file in

  let output_data d name =
    let fname = Filename.concat !dir (name ^ ".mar") in
    let outch = open_out_bin fname in
    Marshal.to_channel outch d [] in

  (* Processing Boolean properties *)
  let proc_bool_prop (prop, prop_name) =
    let tbl = convert_boolean_prop_to_tbl ucd.repertoire prop in
    output_data tbl prop_name in
  List.iter proc_bool_prop uucd_boolprops;

  (* Processing string properties *)
  let module P = struct
    type t = string end in
  let module Tbl = GenTbl(P) in
  let proc_string_property (prop, prop_name) =
    let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
        prop in
    output_data tbl prop_name in
  let string_props = [
    (Uucd.iso_comment, "iso_comment");
    (Uucd.jamo_short_name, "jamo_short_name");
    (Uucd.unicode_1_name, "unicode_1_name");
    (Uucd.kAccountingNumeric, "kAccountingNumeric");
    (Uucd.kAlternateHanYu, "kAlternateHanYu");
    (Uucd.kAlternateJEF, "kAlternateJEF");
    (Uucd.kAlternateKangXi, "kAlternateKangXi");
    (Uucd.kAlternateMorohashi, "kAlternateMorohashi");
    (Uucd.kBigFive, "kBigFive");
    (Uucd.kCCCII, "kCCCII");
    (Uucd.kCNS1986, "kCNS1986");
    (Uucd.kCNS1992, "kCNS1992");
    (Uucd.kCangjie, "kCangjie");
    (Uucd.kCantonese, "kCantonese");
    (Uucd.kCheungBauer, "kCheungBauer");
    (Uucd.kCheungBauerIndex, "KCheungBauerIndex");
    (Uucd.kCihaiT, "kCihaiT");
    (Uucd.kCompatibilityVariant, "kCompatibilityVariant");
    (Uucd.kCowles, "kCowles");
    (Uucd.kDaeJaweon, "kDaeJaweon");
    (Uucd.kDefinition, "kDefinition");
    (Uucd.kEACC, "kEACC");
    (Uucd.kFenn, "kFenn");
    (Uucd.kFennIndex, "kFennIndex");
    (Uucd.kFourCornerCode, "KFourCornerCode");
    (Uucd.kFrequency, "kFrequency");
    (Uucd.kGB0, "kGB0");
    (Uucd.kGB1, "kGB1");
    (Uucd.kGB3, "kGB3");
    (Uucd.kGB5, "kGB5");
    (Uucd.kGB7, "kGB7");
    (Uucd.kGB8, "kGB8");
    (Uucd.kGSR, "kGSR");
    (Uucd.kGradeLevel, "kGradeLevel");
    (Uucd.kHDZRadBreak, "kHDZRadBreak");
    (Uucd.kHKGlyph, "kHKGlyph");
    (Uucd.kHKSCS, "kHKSCS");
    (Uucd.kHanYu, "kHanYu");
    (Uucd.kHangul, "kHangul");
    (Uucd.kHanyuPinlu, "kHanyuPinlu");
    (Uucd.kHanyuPinyin, "kHanyuPinyin");
    (Uucd.kIBMJapan, "kIBMJapan");
    (Uucd.kIICore, "kIICore");
    (Uucd.kIRGDaeJaweon, "kIRGDaeJaweon");
    (Uucd.kIRGDaiKanwaZiten, "kIRGDaiKanwaZiten");
    (Uucd.kIRGHanyuDaZidian, "kIRGHanyuDaZidian");
    (Uucd.kIRGKangXi, "kIRGKangXi");
    (Uucd.kIRG_GSource, "kIRG_GSource");
    (Uucd.kIRG_HSource, "kIRG_HSource");
    (Uucd.kIRG_JSource, "kIRG_JSource");
    (Uucd.kIRG_KPSource, "kIRG_KPSource");
    (Uucd.kIRG_KSource, "kIRG_KSource");
    (Uucd.kIRG_MSource, "kIRG_MSource");
    (Uucd.kIRG_TSource, "kIRG_TSource");
    (Uucd.kIRG_USource, "kIRG_USource");
    (Uucd.kIRG_VSource, "kIRG_VSource");
    (Uucd.kJHJ, "kJHJ");
    (Uucd.kJIS0213, "kJIS0213");
    (Uucd.kJa, "kJa");
    (Uucd.kJapaneseKun, "kJapaneseKun");
    (Uucd.kJapaneseOn, "kJapaneseOn");
    (Uucd.kJis0, "kJis0");
    (Uucd.kJis1, "kJis1");
    (Uucd.kKPS0, "kKPS0");
    (Uucd.kKPS1, "kKPS1");
    (Uucd.kKSC0, "kKSC0");
    (Uucd.kKSC1, "kKSC1");
    (Uucd.kKangXi, "kKangXi");
    (Uucd.kKarlgren, "kKarlgren");
    (Uucd.kKorean, "kKorean");
    (Uucd.kLau, "kLau");
    (Uucd.kMainlandTelegraph, "kMainlandTelegraph");
    (Uucd.kMandarin, "kMandarin");
    (Uucd.kMatthews, "kMatthews");
    (Uucd.kMeyerWempe, "kMeyerWempe");
    (Uucd.kMorohashi, "kMorohashi");
    (Uucd.kNelson, "kNelson");
    (Uucd.kOtherNumeric, "kOtherNumeric");
    (Uucd.kPhonetic, "kPhonetic");
    (Uucd.kPrimaryNumeric, "kPrimaryNumeric");
    (Uucd.kPseudoGB1, "kPseudoGB1");
    (Uucd.kRSAdobe_Japan1_6, "kRSAdobe_Japan1_6");
    (Uucd.kRSJapanese, "kRSJapanese");
    (Uucd.kRSKanWa, "kRSKanWa");
    (Uucd.kRSKangXi, "kRSKangXi");
    (Uucd.kRSKorean, "kRSKorean");
    (Uucd.kRSMerged, "kRSMerged");
    (Uucd.kRSTUnicode, "kRSTUnicode");
    (Uucd.kRSUnicode, "kRSUnicode");
    (Uucd.kReading, "kReading");
    (Uucd.kSBGY, "kSBGY");
    (Uucd.kSemanticVariant, "kSemanticVariant");
    (Uucd.kSimplifiedVariant, "kSimplifiedVariant");
    (Uucd.kSpecializedSemanticVariant, "kSpecializedSemanticVariant");
    (Uucd.kSrc_NushuDuben, "kSrc_NushuDuben");
    (Uucd.kTGT_MergedSrc, "kTGT_MergedSrc");
    (Uucd.kTaiwanTelegraph, "kTaiwanTelegraph");
    (Uucd.kTang, "kTang");
    (Uucd.kTotalStrokes, "kTotalStrokes");
    (Uucd.kTraditionalVariant, "kTraditionalVariant");
    (Uucd.kVietnamese, "kVietnamese");
    (Uucd.kWubi, "kWubi");
    (Uucd.kXHC1983, "kXHC1983");
    (Uucd.kXerox, "kXerox");
    (Uucd.kZVariant, "kZVariant");
  ] in
  List.iter proc_string_property string_props;

  let module BidiClass = struct
    type t = [ `AL
             | `AN
             | `B
             | `BN
             | `CS
             | `EN
             | `ES
             | `ET
             | `FSI
             | `L
             | `LRE
             | `LRI
             | `LRO
             | `NSM
             | `ON
             | `PDF
             | `PDI
             | `R
             | `RLE
             | `RLI
             | `RLO
             | `S
             | `WS ]
  end in
  let module BidiClassTbl = GenVariantTbl(BidiClass) in
  let tbl = BidiClassTbl.convert_prop_to_tbl ucd.repertoire Uucd.bidi_class in
  output_data tbl "bidi_class";

  let module BidiPairedBracket = struct
    type t = [ `Cp of Uucd.cp | `Self ] end in
  let module BidiPairedBracketTbl = GenTbl(BidiPairedBracket) in
  let m = BidiPairedBracketTbl.convert_prop_to_map ucd.repertoire
      Uucd.bidi_paired_bracket in
  let m' = UMap.map (function
        Some (`Cp n) -> Some (`Cp (UChar.of_int n))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
      type t = [ `Cp of UChar.t | `Self ] end in
  let module Tbl = UCharTbl.Make(BidiPairedBracketTbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "bidi_paired_bracket";

  let module P = struct
    type t = [ `C | `N | `O ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.bidi_paired_bracket_type in
  output_data tbl "bidi_paired_bracket_type";

  let module P = struct
    type t = [ `ASCII
             | `Adlam
             | `Aegean_Numbers
             | `Ahom
             | `Alchemical
             | `Alphabetic_PF
             | `Anatolian_Hieroglyphs
             | `Ancient_Greek_Music
             | `Ancient_Greek_Numbers
             | `Ancient_Symbols
             | `Arabic
             | `Arabic_Ext_A
             | `Arabic_Math
             | `Arabic_PF_A
             | `Arabic_PF_B
             | `Arabic_Sup
             | `Armenian
             | `Arrows
             | `Avestan
             | `Balinese
             | `Bamum
             | `Bamum_Sup
             | `Bassa_Vah
             | `Batak
             | `Bengali
             | `Bhaiksuki
             | `Block_Elements
             | `Bopomofo
             | `Bopomofo_Ext
             | `Box_Drawing
             | `Brahmi
             | `Braille
             | `Buginese
             | `Buhid
             | `Byzantine_Music
             | `CJK
             | `CJK_Compat
             | `CJK_Compat_Forms
             | `CJK_Compat_Ideographs
             | `CJK_Compat_Ideographs_Sup
             | `CJK_Ext_A
             | `CJK_Ext_B
             | `CJK_Ext_C
             | `CJK_Ext_D
             | `CJK_Ext_E
             | `CJK_Ext_F
             | `CJK_Radicals_Sup
             | `CJK_Strokes
             | `CJK_Symbols
             | `Carian
             | `Caucasian_Albanian
             | `Chakma
             | `Cham
             | `Cherokee
             | `Cherokee_Sup
             | `Compat_Jamo
             | `Control_Pictures
             | `Coptic
             | `Coptic_Epact_Numbers
             | `Counting_Rod
             | `Cuneiform
             | `Cuneiform_Numbers
             | `Currency_Symbols
             | `Cypriot_Syllabary
             | `Cyrillic
             | `Cyrillic_Ext_A
             | `Cyrillic_Ext_B
             | `Cyrillic_Ext_C
             | `Cyrillic_Sup
             | `Deseret
             | `Devanagari
             | `Devanagari_Ext
             | `Diacriticals
             | `Diacriticals_Ext
             | `Diacriticals_For_Symbols
             | `Diacriticals_Sup
             | `Dingbats
             | `Domino
             | `Duployan
             | `Early_Dynastic_Cuneiform
             | `Egyptian_Hieroglyphs
             | `Elbasan
             | `Emoticons
             | `Enclosed_Alphanum
             | `Enclosed_Alphanum_Sup
             | `Enclosed_CJK
             | `Enclosed_Ideographic_Sup
             | `Ethiopic
             | `Ethiopic_Ext
             | `Ethiopic_Ext_A
             | `Ethiopic_Sup
             | `Geometric_Shapes
             | `Geometric_Shapes_Ext
             | `Georgian
             | `Georgian_Sup
             | `Glagolitic
             | `Glagolitic_Sup
             | `Gothic
             | `Grantha
             | `Greek
             | `Greek_Ext
             | `Gujarati
             | `Gurmukhi
             | `Half_And_Full_Forms
             | `Half_Marks
             | `Hangul
             | `Hanunoo
             | `Hatran
             | `Hebrew
             | `High_PU_Surrogates
             | `High_Surrogates
             | `Hiragana
             | `IDC
             | `IPA_Ext
             | `Ideographic_Symbols
             | `Imperial_Aramaic
             | `Indic_Number_Forms
             | `Inscriptional_Pahlavi
             | `Inscriptional_Parthian
             | `Jamo
             | `Jamo_Ext_A
             | `Jamo_Ext_B
             | `Javanese
             | `Kaithi
             | `Kana_Ext_A
             | `Kana_Sup
             | `Kanbun
             | `Kangxi
             | `Kannada
             | `Katakana
             | `Katakana_Ext
             | `Kayah_Li
             | `Kharoshthi
             | `Khmer
             | `Khmer_Symbols
             | `Khojki
             | `Khudawadi
             | `Lao
             | `Latin_1_Sup
             | `Latin_Ext_A
             | `Latin_Ext_Additional
             | `Latin_Ext_B
             | `Latin_Ext_C
             | `Latin_Ext_D
             | `Latin_Ext_E
             | `Lepcha
             | `Letterlike_Symbols
             | `Limbu
             | `Linear_A
             | `Linear_B_Ideograms
             | `Linear_B_Syllabary
             | `Lisu
             | `Low_Surrogates
             | `Lycian
             | `Lydian
             | `Mahajani
             | `Mahjong
             | `Malayalam
             | `Mandaic
             | `Manichaean
             | `Marchen
             | `Masaram_Gondi
             | `Math_Alphanum
             | `Math_Operators
             | `Meetei_Mayek
             | `Meetei_Mayek_Ext
             | `Mende_Kikakui
             | `Meroitic_Cursive
             | `Meroitic_Hieroglyphs
             | `Miao
             | `Misc_Arrows
             | `Misc_Math_Symbols_A
             | `Misc_Math_Symbols_B
             | `Misc_Pictographs
             | `Misc_Symbols
             | `Misc_Technical
             | `Modi
             | `Modifier_Letters
             | `Modifier_Tone_Letters
             | `Mongolian
             | `Mongolian_Sup
             | `Mro
             | `Multani
             | `Music
             | `Myanmar
             | `Myanmar_Ext_A
             | `Myanmar_Ext_B
             | `NB
             | `NKo
             | `Nabataean
             | `New_Tai_Lue
             | `Newa
             | `Number_Forms
             | `Nushu
             | `OCR
             | `Ogham
             | `Ol_Chiki
             | `Old_Hungarian
             | `Old_Italic
             | `Old_North_Arabian
             | `Old_Permic
             | `Old_Persian
             | `Old_South_Arabian
             | `Old_Turkic
             | `Oriya
             | `Ornamental_Dingbats
             | `Osage
             | `Osmanya
             | `PUA
             | `Pahawh_Hmong
             | `Palmyrene
             | `Pau_Cin_Hau
             | `Phags_Pa
             | `Phaistos
             | `Phoenician
             | `Phonetic_Ext
             | `Phonetic_Ext_Sup
             | `Playing_Cards
             | `Psalter_Pahlavi
             | `Punctuation
             | `Rejang
             | `Rumi
             | `Runic
             | `Samaritan
             | `Saurashtra
             | `Sharada
             | `Shavian
             | `Shorthand_Format_Controls
             | `Siddham
             | `Sinhala
             | `Sinhala_Archaic_Numbers
             | `Small_Forms
             | `Sora_Sompeng
             | `Soyombo
             | `Specials
             | `Sundanese
             | `Sundanese_Sup
             | `Sup_Arrows_A
             | `Sup_Arrows_B
             | `Sup_Arrows_C
             | `Sup_Math_Operators
             | `Sup_PUA_A
             | `Sup_PUA_B
             | `Sup_Punctuation
             | `Sup_Symbols_And_Pictographs
             | `Super_And_Sub
             | `Sutton_SignWriting
             | `Syloti_Nagri
             | `Syriac
             | `Syriac_Sup
             | `Tagalog
             | `Tagbanwa
             | `Tags
             | `Tai_Le
             | `Tai_Tham
             | `Tai_Viet
             | `Tai_Xuan_Jing
             | `Takri
             | `Tamil
             | `Tangut
             | `Tangut_Components
             | `Telugu
             | `Thaana
             | `Thai
             | `Tibetan
             | `Tifinagh
             | `Tirhuta
             | `Transport_And_Map
             | `UCAS
             | `UCAS_Ext
             | `Ugaritic
             | `VS
             | `VS_Sup
             | `Vai
             | `Vedic_Ext
             | `Vertical_Forms
             | `Warang_Citi
             | `Yi_Radicals
             | `Yi_Syllables
             | `Yijing
             | `Zanabazar_Square ]
  end in
  let module Tbl = GenTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.block in
  output_data tbl "block";

  let tbl = convert_byte_prop_to_tbl ucd.repertoire
      Uucd.canonical_combining_class in
  output_data tbl "canonical_combining_class";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.case_folding in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
      type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "case_folding";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(BidiPairedBracket) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.decomposition_mapping in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "decomposition_mapping";

  let module P = struct
    type t = [ `Can
             | `Com
             | `Enc
             | `Fin
             | `Font
             | `Fra
             | `Init
             | `Iso
             | `Med
             | `Nar
             | `Nb
             | `None
             | `Sml
             | `Sqr
             | `Sub
             | `Sup
             | `Vert
             | `Wide ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.decomposition_type in
  output_data tbl "decomposition_type";

  let module P = struct
    type t = [ `A | `F | `H | `N | `Na | `W ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.east_asian_width in
  output_data tbl "east_asian_width";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.fc_nfkc_closure in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "fc_nfkc_closure";

  let module P = struct
    type t = [ `Cc
             | `Cf
             | `Cn
             | `Co
             | `Cs
             | `Ll
             | `Lm
             | `Lo
             | `Lt
             | `Lu
             | `Mc
             | `Me
             | `Mn
             | `Nd
             | `Nl
             | `No
             | `Pc
             | `Pd
             | `Pe
             | `Pf
             | `Pi
             | `Po
             | `Ps
             | `Sc
             | `Sk
             | `Sm
             | `So
             | `Zl
             | `Zp
             | `Zs ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.general_category in
  output_data tbl "general_category";

  let module P = struct
    type t = [ `CN
             | `CR
             | `EB
             | `EBG
             | `EM
             | `EX
             | `GAZ
             | `L
             | `LF
             | `LV
             | `LVT
             | `PP
             | `RI
             | `SM
             | `T
             | `V
             | `XX
             | `ZWJ ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.grapheme_cluster_break in
  output_data tbl "grapheme_cluster_break";

  let module P = struct
    type t = [ `L | `LV | `LVT | `NA | `T | `V ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.hangul_syllable_type in
  output_data tbl "hangul_syllable_type";

  let module P = struct
    type t = [ `Avagraha
             | `Bindu
             | `Brahmi_Joining_Number
             | `Cantillation_Mark
             | `Consonant
             | `Consonant_Dead
             | `Consonant_Final
             | `Consonant_Head_Letter
             | `Consonant_Killer
             | `Consonant_Medial
             | `Consonant_Placeholder
             | `Consonant_Preceding_Repha
             | `Consonant_Prefixed
             | `Consonant_Repha
             | `Consonant_Subjoined
             | `Consonant_Succeeding_Repha
             | `Consonant_With_Stacker
             | `Gemination_Mark
             | `Invisible_Stacker
             | `Joiner
             | `Modifying_Letter
             | `Non_Joiner
             | `Nukta
             | `Number
             | `Number_Joiner
             | `Other
             | `Pure_Killer
             | `Register_Shifter
             | `Syllable_Modifier
             | `Tone_Letter
             | `Tone_Mark
             | `Virama
             | `Visarga
             | `Vowel
             | `Vowel_Dependent
             | `Vowel_Independent ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.indic_syllabic_category in
  output_data tbl "indic_syllabic_category";

  let module P = struct
    type t = [ `Bottom
             | `Bottom_And_Right
             | `Invisible
             | `Left
             | `Left_And_Right
             | `NA
             | `Overstruck
             | `Right
             | `Top
             | `Top_And_Bottom
             | `Top_And_Bottom_And_Right
             | `Top_And_Left
             | `Top_And_Left_And_Right
             | `Top_And_Right
             | `Visual_Order_Left ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.indic_matra_category in
  output_data tbl "indic_matra_category";

  let module P = struct
    type t = [ `Bottom
             | `Bottom_And_Right
             | `Left
             | `Left_And_Right
             | `NA
             | `Overstruck
             | `Right
             | `Top
             | `Top_And_Bottom
             | `Top_And_Bottom_And_Right
             | `Top_And_Left
             | `Top_And_Left_And_Right
             | `Top_And_Right
             | `Visual_Order_Left ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.indic_positional_category in
  output_data tbl "indic_positional_category";

  let module P = struct
    type t = [ `African_Feh
             | `African_Noon
             | `African_Qaf
             | `Ain
             | `Alaph
             | `Alef
             | `Alef_Maqsurah
             | `Beh
             | `Beth
             | `Burushaski_Yeh_Barree
             | `Dal
             | `Dalath_Rish
             | `E
             | `Farsi_Yeh
             | `Fe
             | `Feh
             | `Final_Semkath
             | `Gaf
             | `Gamal
             | `Hah
             | `Hamza_On_Heh_Goal
             | `He
             | `Heh
             | `Heh_Goal
             | `Heth
             | `Kaf
             | `Kaph
             | `Khaph
             | `Knotted_Heh
             | `Lam
             | `Lamadh
             | `Malayalam_Bha
             | `Malayalam_Ja
             | `Malayalam_Lla
             | `Malayalam_Llla
             | `Malayalam_Nga
             | `Malayalam_Nna
             | `Malayalam_Nnna
             | `Malayalam_Nya
             | `Malayalam_Ra
             | `Malayalam_Ssa
             | `Malayalam_Tta
             | `Manichaean_Aleph
             | `Manichaean_Ayin
             | `Manichaean_Beth
             | `Manichaean_Daleth
             | `Manichaean_Dhamedh
             | `Manichaean_Five
             | `Manichaean_Gimel
             | `Manichaean_Heth
             | `Manichaean_Hundred
             | `Manichaean_Kaph
             | `Manichaean_Lamedh
             | `Manichaean_Mem
             | `Manichaean_Nun
             | `Manichaean_One
             | `Manichaean_Pe
             | `Manichaean_Qoph
             | `Manichaean_Resh
             | `Manichaean_Sadhe
             | `Manichaean_Samekh
             | `Manichaean_Taw
             | `Manichaean_Ten
             | `Manichaean_Teth
             | `Manichaean_Thamedh
             | `Manichaean_Twenty
             | `Manichaean_Waw
             | `Manichaean_Yodh
             | `Manichaean_Zayin
             | `Meem
             | `Mim
             | `No_Joining_Group
             | `Noon
             | `Nun
             | `Nya
             | `Pe
             | `Qaf
             | `Qaph
             | `Reh
             | `Reversed_Pe
             | `Rohingya_Yeh
             | `Sad
             | `Sadhe
             | `Seen
             | `Semkath
             | `Shin
             | `Straight_Waw
             | `Swash_Kaf
             | `Syriac_Waw
             | `Tah
             | `Taw
             | `Teh_Marbuta
             | `Teh_Marbuta_Goal
             | `Teth
             | `Waw
             | `Yeh
             | `Yeh_Barree
             | `Yeh_With_Tail
             | `Yudh
             | `Yudh_He
             | `Zain
             | `Zhain ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.joining_group in
  output_data tbl "joining_group";

  let module P = struct
    type t = [ `AI
             | `AL
             | `B2
             | `BA
             | `BB
             | `BK
             | `CB
             | `CJ
             | `CL
             | `CM
             | `CP
             | `CR
             | `EB
             | `EM
             | `EX
             | `GL
             | `H2
             | `H3
             | `HL
             | `HY
             | `ID
             | `IN
             | `IS
             | `JL
             | `JT
             | `JV
             | `LF
             | `NL
             | `NS
             | `NU
             | `OP
             | `PO
             | `PR
             | `QU
             | `RI
             | `SA
             | `SG
             | `SP
             | `SY
             | `WJ
             | `XX
             | `ZW
             | `ZWJ ]
  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.line_break in
  output_data tbl "line_break";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.lowercase_mapping in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "lowercase_mapping";

  let module P = struct
    type t = [ `Name of string | `Pattern of string ] end in
  let module Tbl = GenTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.name in
  output_data tbl "name";

  let module P = struct
    type t =
      (string *
       [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ])
       list end in
  let module Tbl = GenTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.name_alias in
  output_data tbl "name_alias";

  let module P = struct
    type t = [ `False | `Maybe | `True ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.nfc_quick_check in
  output_data tbl "nfc_quick_check";

  let module P = struct
    type t = [ `False | `Maybe | `True ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.nfd_quick_check in
  output_data tbl "nfd_quick_check";

  let module P = struct
    type t = [ `False | `Maybe | `True ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.nfkc_quick_check in
  output_data tbl "nfkc_quick_check";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.nfkc_casefold in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "nfkc_casefold";

  let module P = struct
    type t = [ `False | `Maybe | `True ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.nfkd_quick_check in
  output_data tbl "nfkd_quick_check";

  let module P = struct
    type t = [ `De | `Di | `None | `Nu ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.numeric_type in
  output_data tbl "numeric_type";

  let module P = struct
    type t =[ `Frac of int * int | `NaN | `Num of int64 ] end in
  let module Tbl = GenTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.numeric_value in
  output_data tbl "numeric_value";

  let module P = struct
    type t = [ `Adlm
       | `Aghb
       | `Ahom
       | `Arab
       | `Armi
       | `Armn
       | `Avst
       | `Bali
       | `Bamu
       | `Bass
       | `Batk
       | `Beng
       | `Bhks
       | `Bopo
       | `Brah
       | `Brai
       | `Bugi
       | `Buhd
       | `Cakm
       | `Cans
       | `Cari
       | `Cham
       | `Cher
       | `Copt
       | `Cprt
       | `Cyrl
       | `Deva
       | `Dsrt
       | `Dupl
       | `Egyp
       | `Elba
       | `Ethi
       | `Geor
       | `Glag
       | `Gonm
       | `Goth
       | `Gran
       | `Grek
       | `Gujr
       | `Guru
       | `Hang
       | `Hani
       | `Hano
       | `Hatr
       | `Hebr
       | `Hira
       | `Hluw
       | `Hmng
       | `Hrkt
       | `Hung
       | `Ital
       | `Java
       | `Kali
       | `Kana
       | `Khar
       | `Khmr
       | `Khoj
       | `Knda
       | `Kthi
       | `Lana
       | `Laoo
       | `Latn
       | `Lepc
       | `Limb
       | `Lina
       | `Linb
       | `Lisu
       | `Lyci
       | `Lydi
       | `Mahj
       | `Mand
       | `Mani
       | `Marc
       | `Mend
       | `Merc
       | `Mero
       | `Mlym
       | `Modi
       | `Mong
       | `Mroo
       | `Mtei
       | `Mult
       | `Mymr
       | `Narb
       | `Nbat
       | `Newa
       | `Nkoo
       | `Nshu
       | `Ogam
       | `Olck
       | `Orkh
       | `Orya
       | `Osge
       | `Osma
       | `Palm
       | `Pauc
       | `Perm
       | `Phag
       | `Phli
       | `Phlp
       | `Phnx
       | `Plrd
       | `Prti
       | `Qaai
       | `Rjng
       | `Runr
       | `Samr
       | `Sarb
       | `Saur
       | `Sgnw
       | `Shaw
       | `Shrd
       | `Sidd
       | `Sind
       | `Sinh
       | `Sora
       | `Soyo
       | `Sund
       | `Sylo
       | `Syrc
       | `Tagb
       | `Takr
       | `Tale
       | `Talu
       | `Taml
       | `Tang
       | `Tavt
       | `Telu
       | `Tfng
       | `Tglg
       | `Thaa
       | `Thai
       | `Tibt
       | `Tirh
       | `Ugar
       | `Vaii
       | `Wara
       | `Xpeo
       | `Xsux
       | `Yiii
       | `Zanb
       | `Zinh
       | `Zyyy
       | `Zzzz ]  end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.script in
  output_data tbl "script";

  let module P = struct
    type t = [ `AT
             | `CL
             | `CR
             | `EX
             | `FO
             | `LE
             | `LF
             | `LO
             | `NU
             | `SC
             | `SE
             | `SP
             | `ST
             | `UP
             | `XX ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.sentence_break in
  output_data tbl "sentence_break";

  let module P = struct
    type t = [ `Cp of Uucd.cp | `Self ] end in
  let module GTbl = GenTbl(P) in
  let m = GTbl.convert_prop_to_map ucd.repertoire
      Uucd.simple_case_folding in
  let m' = UMap.map (function
        Some (`Cp n) -> Some (`Cp (UChar.of_int n))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cp of UChar.t | `Self ] end in
  let module Tbl = UCharTbl.Make(GTbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "simple_case_folding";

  let module P = struct
    type t = [ `Cp of Uucd.cp | `Self ] end in
  let module GTbl = GenTbl(P) in
  let m = GTbl.convert_prop_to_map ucd.repertoire
      Uucd.simple_lowercase_mapping in
  let m' = UMap.map (function
        Some (`Cp n) -> Some (`Cp (UChar.of_int n))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cp of UChar.t | `Self ] end in
  let module Tbl = UCharTbl.Make(GTbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "simple_lowercase_mapping";

  let module P = struct
    type t = [ `Cp of Uucd.cp | `Self ] end in
  let module GTbl = GenTbl(P) in
  let m = GTbl.convert_prop_to_map ucd.repertoire
      Uucd.simple_titlecase_mapping in
  let m' = UMap.map (function
        Some (`Cp n) -> Some (`Cp (UChar.of_int n))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cp of UChar.t | `Self ] end in
  let module Tbl = UCharTbl.Make(GTbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "simple_titlecase_mapping";

  let module P = struct
    type t = [ `Cp of Uucd.cp | `Self ] end in
  let module GTbl = GenTbl(P) in
  let m = GTbl.convert_prop_to_map ucd.repertoire
      Uucd.simple_uppercase_mapping in
  let m' = UMap.map (function
        Some (`Cp n) -> Some (`Cp (UChar.of_int n))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cp of UChar.t | `Self ] end in
  let module Tbl = UCharTbl.Make(GTbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "simple_uppercase_mapping";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.titlecase_mapping in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "titlecase_mapping";

  let module P = struct
    type t = [ `Char | `Noncharacter | `Reserved | `Surrogate ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.uax_42_element in
  output_data tbl "uax_42_element";

  let module P = struct
    type t = [ `Cps of Uucd.cp list | `Self ] end in
  let module Tbl = GenTbl(P) in
  let m = Tbl.convert_prop_to_map ucd.repertoire
      Uucd.uppercase_mapping in
  let m' = UMap.map (function
        Some (`Cps ns) -> Some (`Cps (List.map UChar.of_int ns))
      | Some `Self -> Some `Self
      | None -> None) m in
  let module P = struct
    type t = [ `Cps of UChar.t list | `Self ] end in
  let module Tbl = UCharTbl.Make(Tbl.Option(P)) in
  let tbl = Tbl.of_map None m' in
  output_data (Unidata.Any tbl) "uppercase_mapping";

  let module P = struct
    type t = [ `R | `Tr | `Tu | `U ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.vertical_orientation in
  output_data tbl "vertical_orientation";

  let module P = struct
    type t = [ `CR
             | `DQ
             | `EB
             | `EBG
             | `EM
             | `EX
             | `Extend
             | `FO
             | `GAZ
             | `HL
             | `KA
             | `LE
             | `LF
             | `MB
             | `ML
             | `MN
             | `NL
             | `NU
             | `RI
             | `SQ
             | `XX
             | `ZWJ ] end in
  let module Tbl = GenVariantTbl(P) in
  let tbl = Tbl.convert_prop_to_tbl ucd.repertoire
      Uucd.word_break in
  output_data tbl "word_break";
