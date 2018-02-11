(** Unicode character informations *)
(* Copyright (C) 2002, 2003, 2018 Yamagata Yoriyuki.*)
(*               2010 Pierre Chambart *)

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

module type Type = sig

  type 'a prop
  type 'a data

  val get_data : 'a prop -> 'a data
  val get_value : 'a data -> UChar.t -> 'a
  val get_set : bool prop -> USet.t
  val get_map : 'a prop -> 'a UMap.t

  (** Character Information *)
  type age_type = [ `Version of int * int | `Unassigned ]
  val age_prop :  age_type prop

  (** [older v1 v2] is [true] if [v1] is older ( or the same version )
      than [v2]. Everithing is older than [`Nc] *)
  val older : age_type -> age_type -> bool


  val alphabetic_prop : bool prop
  val ascii_hex_digit_prop : bool prop

  type bidi_class_type = [
    | `AL
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
    | `WS
  ]
  val bidi_class_prop : bidi_class_type prop

  val bidi_control_prop : bool prop
  val bidi_mirrored_prop : bool prop
  val bidi_mirroring_glyph_prop : UChar.t option prop
  val bidi_paired_bracket_prop : [ `Self | `Cp of UChar.t ] prop

  type bidi_paired_bracket_type_type = [ `O | `C | `N ]
  val bidi_paired_bracket_type_prop :  bidi_paired_bracket_type_type prop

  type block_type =
    [ `Adlam
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
    | `ASCII
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
    | `Carian
    | `Caucasian_Albanian
    | `Chakma
    | `Cham
    | `Cherokee
    | `Cherokee_Sup
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
    | `Diacriticals_For_Symbols
    | `Diacriticals_Sup
    | `Diacriticals_Ext
    | `Dingbats
    | `Domino
    | `Duployan
    | `Early_Dynastic_Cuneiform
    | `Egyptian_Hieroglyphs
    | `Emoticons
    | `Enclosed_Alphanum
    | `Enclosed_Alphanum_Sup
    | `Enclosed_CJK
    | `Enclosed_Ideographic_Sup
    | `Ethiopic
    | `Ethiopic_Ext
    | `Ethiopic_Ext_A
    | `Ethiopic_Sup
    | `Elbasan
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
    | `Ideographic_Symbols
    | `Imperial_Aramaic
    | `Indic_Number_Forms
    | `Inscriptional_Pahlavi
    | `Inscriptional_Parthian
    | `IPA_Ext
    | `Jamo
    | `Jamo_Ext_A
    | `Jamo_Ext_B
    | `Javanese
    | `Kaithi
    | `Kana_Sup
    | `Kanbun
    | `Kangxi
    | `Kannada
    | `Katakana
    | `Katakana_Ext
    | `Kana_Ext_A
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
    | `Music
    | `Multani
    | `Myanmar
    | `Myanmar_Ext_A
    | `Myanmar_Ext_B
    | `Nabataean
    | `NB
    | `New_Tai_Lue
    | `Newa
    | `NKo
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
    | `PUA
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
    | `Vai
    | `Vedic_Ext
    | `Vertical_Forms
    | `VS
    | `VS_Sup
    | `Warang_Citi
    | `Yi_Radicals
    | `Yi_Syllables
    | `Yijing
    | `Zanabazar_Square
    ]
  val block_prop : block_type prop

  val canonical_combining_class_prop : char prop
  val cased_prop : bool prop
  val case_folding_prop : [`Self | `Cps of UChar.t list ] prop
  val case_ignorable_prop : bool prop
  val changes_when_casefolded_prop : bool prop
  val changes_when_casemapped_prop : bool prop
  val changes_when_lowercased_prop : bool prop
  val changes_when_nfkc_casefolded_prop : bool prop
  val changes_when_titlecased_prop : bool prop
  val changes_when_uppercased_prop : bool prop
  val composition_exclusion_prop : bool prop
  val dash_prop : bool prop
  val decomposition_mapping_prop : [`Self | `Cps of UChar.t list ] prop

  type decomposition_type_type = [
    | `Can
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
    | `Sml
    | `Sqr
    | `Sub
    | `Sup
    | `Vert
    | `Wide
    | `None
  ]
  val decomposition_type_prop : decomposition_type_type prop

  val default_ignorable_code_point_prop : bool prop
  val deprecated_prop : bool prop
  val diacritic_prop : bool prop

  type east_asian_width_type = [ `A | `F | `H | `N | `Na | `W ]
  val east_asian_width_prop :  east_asian_width_type prop

  val expands_on_nfc_prop : bool prop
  val expands_on_nfd_prop : bool prop
  val expands_on_nfkc_prop : bool prop
  val expands_on_nfkd_prop : bool prop
  val extender_prop : bool prop
  val fc_nfkc_closure_prop : [ `Self | `Cps of UChar.t list ] prop
  val full_composition_exclusion_prop : bool prop

  type general_category_type = [
    | `Lu
    | `Ll
    | `Lt
    | `Lm
    | `Lo
    | `Mn
    | `Mc
    | `Me
    | `Nd
    | `Nl
    | `No
    | `Pc
    | `Pd
    | `Ps
    | `Pe
    | `Pi
    | `Pf
    | `Po
    | `Sm
    | `Sc
    | `Sk
    | `So
    | `Zs
    | `Zl
    | `Zp
    | `Cc
    | `Cf
    | `Cs
    | `Co
    | `Cn
  ]
  val general_category_prop : general_category_type prop

  val grapheme_base_prop : bool prop

  type grapheme_cluster_break_type = [
    | `CN
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
    | `ZWJ
  ]
  val grapheme_cluster_break_prop : grapheme_cluster_break_type prop

  val grapheme_extend_prop : bool prop
  val grapheme_link_prop : bool prop

  type hangul_syllable_type_type = [ `L | `LV | `LVT | `T | `V | `NA ]
  val hangul_syllable_type_prop : hangul_syllable_type_type prop

  val hex_digit_prop : bool prop
  val hyphen_prop : bool prop
  val id_continue_prop : bool prop
  val id_start_prop : bool prop
  val ideographic_prop : bool prop
  val ids_binary_operator_prop : bool prop
  val ids_trinary_operator_prop : bool prop

  type indic_syllabic_category_type = [
    | `Avagraha
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
    | `Vowel_Independent
  ]
  val indic_syllabic_category_prop : indic_syllabic_category_type prop

  type indic_matra_category_type = [
    | `Right
    | `Left
    | `Visual_Order_Left
    | `Left_And_Right
    | `Top
    | `Bottom
    | `Top_And_Bottom
    | `Top_And_Right
    | `Top_And_Left
    | `Top_And_Left_And_Right
    | `Bottom_And_Right
    | `Top_And_Bottom_And_Right
    | `Overstruck
    | `Invisible
    | `NA
  ]
  val indic_matra_category_prop : indic_matra_category_type prop

  type indic_positional_category_type = [
    | `Bottom
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
    | `Visual_Order_Left
  ]
  val indic_positional_category_prop : indic_positional_category_type prop

  val iso_comment_prop : string prop
  val jamo_short_name_prop : string prop
  val join_control_prop : bool prop

  type joining_group_type = [
    | `African_Feh
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
    | `Zhain
  ]
  val joining_group_prop : joining_group_type prop

  type joining_type_type = [ `U | `C | `T | `D | `L | `R ]
  val joining_type_prop : joining_type_type prop

  type line_break_type = [
    | `AI
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
    | `EB
    | `EM
    | `ZWJ
  ]
  val line_break_prop : line_break_type prop

  val logical_order_exception_prop : bool prop
  val lowercase_prop : bool prop
  val lowercase_mapping_prop : [`Self | `Cps of UChar.t list ] prop
  val math_prop : bool prop
  val name_prop : [`Pattern of string | `Name of string ] prop
  (** In the [`Pattern] case occurences of the character ['#']
      ([U+0023]) in the string must be replaced by the value of the code
      point as four to six uppercase hexadecimal digits (the minimal
      needed). E.g. the pattern ["CJK UNIFIED IDEOGRAPH-#"] associated
      to code point [U+3400] gives the name ["CJK UNIFIED IDEOGRAPH-3400"].  *)

  val name_alias_prop :
    (string * [`Abbreviation | `Alternate | `Control | `Correction | `Figment])
      list prop

  val nfc_quick_check_prop : [ `True | `False | `Maybe ] prop
  val nfd_quick_check_prop : [ `True | `False | `Maybe ] prop
  val nfkc_quick_check_prop : [ `True | `False | `Maybe ] prop
  val nfkc_casefold_prop : [`Self | `Cps of UChar.t list] prop
  val nfkd_quick_check_prop : [ `True | `False | `Maybe ] prop
  val noncharacter_code_point_prop : bool prop
  val numeric_type_prop : [ `None | `De | `Di | `Nu ] prop
  val numeric_value_prop : [`NaN | `Frac of int * int | `Num of int64] prop
  val other_alphabetic_prop : bool prop
  val other_default_ignorable_code_point_prop : bool prop
  val other_grapheme_extend_prop : bool prop
  val other_id_continue_prop : bool prop
  val other_id_start_prop : bool prop
  val other_lowercase_prop : bool prop
  val other_math_prop : bool prop
  val other_uppercase_prop : bool prop
  val pattern_syntax_prop : bool prop
  val pattern_white_space_prop : bool prop
  val prepended_concatenation_mark_prop : bool prop
  val quotation_mark_prop : bool prop
  val radical_prop : bool prop
  val regional_indicator_prop : bool prop

  type script_type = [
    | `Adlm
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
    | `Zzzz
  ]
  val script_prop : script_type prop
  val script_extensions_prop : script_type list prop

  type sentence_break_type = [
    | `AT
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
    | `XX
  ]
  val sentence_break_prop : sentence_break_type prop

  val simple_case_folding_prop : [ `Self | `Cp of UChar.t ] prop
  val simple_lowercase_mapping_prop : [ `Self | `Cp of UChar.t ] prop
  val simple_titlecase_mapping_prop : [ `Self | `Cp of UChar.t ] prop
  val simple_uppercase_mapping_prop : [ `Self | `Cp of UChar.t ] prop
  val soft_dotted_prop : bool prop
  val sterm_prop : bool prop
  val terminal_punctuation_prop : bool prop
  val titlecase_mapping_prop : [`Self | `Cps of UChar.t list ] prop
  val uax_42_element_prop : [ `Reserved | `Noncharacter | `Surrogate | `Char ] prop
  (** Not normative, artefact of [Uucd]. Corresponds to the
      {{:http://www.unicode.org/reports/tr42/#w1aac13b9b1}XML element name}
      that describes the code point. *)

  val unicode_1_name_prop : string prop
  val unified_ideograph_prop : bool prop
  val uppercase_prop : bool prop
  val uppercase_mapping_prop : [`Self | `Cps of UChar.t list ] prop
  val variation_selector_prop : bool prop
  val vertical_orientation_prop : [ `U | `R | `Tu | `Tr ] prop
  val white_space_prop : bool prop
  type word_break_type = [
    | `CR
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
    | `ZWJ
  ]
  val word_break_prop : word_break_type prop

  val xid_continue_prop : bool prop
  val xid_start_prop : bool prop

  (** {2:unihan Unihan properties}
      In alphabetic order. For now unihan properties are always
      represented as strings. *)

  val kAccountingNumeric_prop : string prop
  val kAlternateHanYu_prop : string prop
  val kAlternateJEF_prop : string prop
  val kAlternateKangXi_prop : string prop
  val kAlternateMorohashi_prop : string prop
  val kBigFive_prop : string prop
  val kCCCII_prop : string prop
  val kCNS1986_prop : string prop
  val kCNS1992_prop : string prop
  val kCangjie_prop : string prop
  val kCantonese_prop : string prop
  val kCheungBauer_prop : string prop
  val kCheungBauerIndex_prop : string prop
  val kCihaiT_prop : string prop
  val kCompatibilityVariant_prop : string prop
  val kCowles_prop : string prop
  val kDaeJaweon_prop : string prop
  val kDefinition_prop : string prop
  val kEACC_prop : string prop
  val kFenn_prop : string prop
  val kFennIndex_prop : string prop
  val kFourCornerCode_prop : string prop
  val kFrequency_prop : string prop
  val kGB0_prop : string prop
  val kGB1_prop : string prop
  val kGB3_prop : string prop
  val kGB5_prop : string prop
  val kGB7_prop : string prop
  val kGB8_prop : string prop
  val kGSR_prop : string prop
  val kGradeLevel_prop : string prop
  val kHDZRadBreak_prop : string prop
  val kHKGlyph_prop : string prop
  val kHKSCS_prop : string prop
  val kHanYu_prop : string prop
  val kHangul_prop : string prop
  val kHanyuPinlu_prop : string prop
  val kHanyuPinyin_prop : string prop
  val kIBMJapan_prop : string prop
  val kIICore_prop : string prop
  val kIRGDaeJaweon_prop : string prop
  val kIRGDaiKanwaZiten_prop : string prop
  val kIRGHanyuDaZidian_prop : string prop
  val kIRGKangXi_prop : string prop
  val kIRG_GSource_prop : string prop
  val kIRG_HSource_prop : string prop
  val kIRG_JSource_prop : string prop
  val kIRG_KPSource_prop : string prop
  val kIRG_KSource_prop : string prop
  val kIRG_MSource_prop : string prop
  val kIRG_TSource_prop : string prop
  val kIRG_USource_prop : string prop
  val kIRG_VSource_prop : string prop
  val kJHJ_prop : string prop
  val kJIS0213_prop : string prop
  val kJa_prop : string prop
  val kJapaneseKun_prop : string prop
  val kJapaneseOn_prop : string prop
  val kJis0_prop : string prop
  val kJis1_prop : string prop
  val kKPS0_prop : string prop
  val kKPS1_prop : string prop
  val kKSC0_prop : string prop
  val kKSC1_prop : string prop
  val kKangXi_prop : string prop
  val kKarlgren_prop : string prop
  val kKorean_prop : string prop
  val kLau_prop : string prop
  val kMainlandTelegraph_prop : string prop
  val kMandarin_prop : string prop
  val kMatthews_prop : string prop
  val kMeyerWempe_prop : string prop
  val kMorohashi_prop : string prop
  val kNelson_prop : string prop
  val kOtherNumeric_prop : string prop
  val kPhonetic_prop : string prop
  val kPrimaryNumeric_prop : string prop
  val kPseudoGB1_prop : string prop
  val kRSAdobe_Japan1_6_prop : string prop
  val kRSJapanese_prop : string prop
  val kRSKanWa_prop : string prop
  val kRSKangXi_prop : string prop
  val kRSKorean_prop : string prop
  val kRSMerged_prop : string prop
  val kRSTUnicode_prop : string prop
  val kRSUnicode_prop : string prop
  val kReading_prop : string prop
  val kSBGY_prop : string prop
  val kSemanticVariant_prop : string prop
  val kSimplifiedVariant_prop : string prop
  val kSpecializedSemanticVariant_prop : string prop
  val kSrc_NushuDuben_prop : string prop
  val kTGT_MergedSrc_prop : string prop
  val kTaiwanTelegraph_prop : string prop
  val kTang_prop : string prop
  val kTotalStrokes_prop : string prop
  val kTraditionalVariant_prop : string prop
  val kVietnamese_prop : string prop
  val kWubi_prop : string prop
  val kXHC1983_prop : string prop
  val kXerox_prop : string prop
  val kZVariant_prop : string prop

  (** Character Information *)

  (** Type of Unicode general character categories.
      Each variant specifies
      - [`Lu] : Letter, Uppercase
      - [`Ll] : Letter, Lowercase
      - [`Lt] : Letter, Titlecase
      - [`Mn] : Mark, Non-Spacing
      - [`Mc] : Mark, Spacing Combining
      - [`Me] : Mark, Enclosing
      - [`Nd] : Number, Decimal Digit
      - [`Nl] : Number, Letter
      - [`No] : Number, Other
      - [`Zs] : Separator, Space
      - [`Zl] : Separator, Line
      - [`Zp] : Separator, Paragraph
      - [`Cc] : Other, Control
      - [`Cf] : Other, Format
      - [`Cs] : Other, Surrogate
      - [`Co] : Other, Private Use
      - [`Cn] : Other, Not Assigned
      - [`Lm] : Letter, Modifier
      - [`Lo] : Letter, Other
      - [`Pc] : Punctuation, Connector
      - [`Pd] : Punctuation, Dash
      - [`Ps] : Punctuation, Open
      - [`Pe] : Punctuation, Close
      - [`Pi] : Punctuation, Initial
      - [`Pf] : Punctuation, Final
      - [`Po] : Punctuation, Other
      - [`Sm] : Symbol, Math
      - [`Sc] : Symbol, Currency
      - [`Sk] : Symbol, Modifier
      - [`So] : Symbol, Other  *)

  (* To be removed *)
  val general_category : UChar.t -> general_category_type
  val load_general_category_map : unit -> general_category_type UMap.t

  (** Type of character properties *)
  type character_property_type =
    [ `Math (**Derived Core Properties*)
    | `Alphabetic
    | `Lowercase
    | `Uppercase
    | `ID_Start
    | `ID_Continue
    | `XID_Start
    | `XID_Continue
    | `Default_Ignorable_Code_Point
    | `Grapheme_Extend
    | `Grapheme_Base

    | `Bidi_Control (**Extended Properties*)
    | `White_Space
    | `Hyphen
    | `Quotation_Mark
    | `Terminal_Punctuation
    | `Other_Math
    | `Hex_Digit
    | `Ascii_Hex_Digit
    | `Other_Alphabetic
    | `Ideographic
    | `Diacritic
    | `Extender
    | `Other_Lowercase
    | `Other_Uppercase
    | `Noncharacter_Code_Point
    | `Other_Grapheme_Extend
    | `Grapheme_Link
    | `IDS_Binary_Operator
    | `IDS_Trinary_Operator
    | `Radical
    | `Unified_Ideograph
    | `Other_default_Ignorable_Code_Point
    | `Deprecated
    | `Soft_Dotted
    | `Logical_Order_Exception ]

  (** Load the table for the given character type. *)
  val load_property_tbl : character_property_type -> UCharTbl.Bool.t

  (** Load the table for the given name of the character type.
      The name can be obtained by removing ` from its name of
      the polymorphic variant tag. *)
  val load_property_tbl_by_name : string -> UCharTbl.Bool.t

  (** Load the set of characters of the given character type. *)
  val load_property_set : character_property_type -> USet.t

  (** Load the set of characters of the given name of the character type.
      The name can be obtained by removing ` from its name of
      the polymorphic variant tag. *)
  val load_property_set_by_name : string -> USet.t


  (* val script : UChar.t -> script_type
     val load_script_map : unit -> script_type UMap.t *)

  (** age *)
  type version_type =
    [ `Nc		(** undefined code point *)
    | `v1_0
    | `v1_1
    | `v2_0
    | `v2_1
    | `v3_0
    | `v3_1
    | `v3_2 ]

  (** [age c] unicode version in wich [c] was introduced *)
  val age : UChar.t -> version_type

  (** [older v1 v2] is [true] if [v1] is older ( or the same version )
      than [v2]. Everithing is older than [`Nc] *)
  (* val older : age_type -> age_type -> bool *)

  (** casing *)

  val load_to_lower1_tbl : unit -> UChar.t UCharTbl.t
  val load_to_upper1_tbl : unit -> UChar.t UCharTbl.t
  val load_to_title1_tbl : unit -> UChar.t UCharTbl.t

  type casemap_condition =
    [ `Locale of string
    | `FinalSigma
    | `AfterSoftDotted
    | `MoreAbove
    | `BeforeDot
    | `Not of casemap_condition ]

  type special_casing_property =
    {lower : UChar.t list;
     title : UChar.t list;
     upper : UChar.t list;
     condition : casemap_condition list;}

  val load_conditional_casing_tbl :
    unit -> special_casing_property list UCharTbl.t

  val load_casefolding_tbl : unit -> UChar.t list UCharTbl.t

  (** Combined class
      A combined class is an integer of 0 -- 255, showing how this character
      interacts to other combined characters.  *)
  val combined_class : UChar.t -> int

  (** Decomposition *)

  (** Types of decomposition. *)
  type decomposition_type =
    [ `Canon | `Font | `NoBreak | `Initial | `Medial | `Final |
      `Isolated | `Circle | `Super | `Sub | `Vertical | `Wide | `Narrow |
      `Small | `Square | `Fraction | `Compat ]

  type decomposition_info =
    [ `Canonform (** Already in the canonical form *)
    | `HangulSyllable (** Hangul is treated algotighmically.*)
    | `Composite of decomposition_type * UChar.t list
      (** [`Composite (dtype, text)] means the given character is decomposed into
          text by dtype decomposition. *)
    ]

  val load_decomposition_tbl : unit -> decomposition_info UCharTbl.t

  (** Canonical Composition *)

  (** The return value [[(u_1, u'_1); ... (u_n, u'_1)]] means
      for the given character [u], [u u_i] forms
      the canonical composition [u'_i].
      If u is a Hangul jamo, composition returns []. *)
  val load_composition_tbl : unit -> (UChar.t * UChar.t) list UCharTbl.t

  (** Whether the given composed character is used in NFC or NFKC *)
  val load_composition_exclusion_tbl : unit -> UCharTbl.Bool.t

end

module Make (Config : ConfigInt.Type) : Type
