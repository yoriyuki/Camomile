(* Copyright (C) 2002, 2018 Yamagata Yoriyuki.*)
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

  (* Old code. To be removed *)
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

  (** Type for script type *)
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

module Make (Config : ConfigInt.Type) : Type = struct
  module UData = Unidata.Make(Config)

  type age_type = [ `Version of int * int | `Unassigned ]
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
  type bidi_paired_bracket_type_type = [ `O | `C | `N ]
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
  type east_asian_width_type = [ `A | `F | `H | `N | `Na | `W ]
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
  type hangul_syllable_type_type = [ `L | `LV | `LVT | `T | `V | `NA ]
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
  type joining_type_type = [ `U | `C | `T | `D | `L | `R ]
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

  type _ prop = [
      `Age
    | `Alphabetic
    | `Ascii_hex_digit
    | `Bidi_class
    | `Bidi_control
    | `Bidi_mirrored
    | `Bidi_mirroring_glyph
    | `Bidi_paired_bracket
    | `Bidi_paired_bracket_type
    | `Block
    | `Canonical_combining_class
    | `Cased
    | `Case_folding
    | `Case_ignorable
    | `Changes_when_casefolded
    | `Changes_when_casemapped
    | `Changes_when_lowercased
    | `Changes_when_nfkc_casefolded
    | `Changes_when_titlecased
    | `Changes_when_uppercased
    | `Composition_exclusion
    | `Dash
    | `Decomposition_mapping
    | `Decomposition_type
    | `Default_ignorable_code_point
    | `Deprecated
    | `Diacritic
    | `East_asian_width
    | `Expands_on_nfc
    | `Expands_on_nfd
    | `Expands_on_nfkc
    | `Expands_on_nfkd
    | `Extender
    | `Fc_nfkc_closure
    | `Full_composition_exclusion
    | `General_category
    | `Grapheme_base
    | `Grapheme_cluster_break
    | `Grapheme_extend
    | `Grapheme_link
    | `Hangul_syllable_type
    | `Hex_digit
    | `Hyphen
    | `Id_continue
    | `Id_start
    | `Ideographic
    | `Ids_binary_operator
    | `Ids_trinary_operator
    | `Indic_syllabic_category
    | `Indic_matra_category
    | `Indic_positional_category
    | `Iso_comment
    | `Jamo_short_name
    | `Join_control
    | `Joining_group
    | `Joining_type
    | `Line_break
    | `Logical_order_exception
    | `Lowercase
    | `Lowercase_mapping
    | `Math
    | `Name
    | `Name_alias
    | `Nfc_quick_check
    | `Nfd_quick_check
    | `Nfkc_quick_check
    | `Nfkc_casefold
    | `Nfkd_quick_check
    | `Noncharacter_code_point
    | `Numeric_type
    | `Numeric_value
    | `Other_alphabetic
    | `Other_default_ignorable_code_point
    | `Other_grapheme_extend
    | `Other_id_continue
    | `Other_id_start
    | `Other_lowercase
    | `Other_math
    | `Other_uppercase
    | `Pattern_syntax
    | `Pattern_white_space
    | `Prepended_concatenation_mark
    | `Quotation_mark
    | `Radical
    | `Regional_indicator
    | `Script
    | `Script_extensions
    | `Sentence_break
    | `Simple_case_folding
    | `Simple_lowercase_mapping
    | `Simple_titlecase_mapping
    | `Simple_uppercase_mapping
    | `Soft_dotted
    | `Sterm
    | `Terminal_punctuation
    | `Titlecase_mapping
    | `Uax_42_element
    | `Unicode_1_name
    | `Unified_ideograph
    | `Uppercase
    | `Uppercase_mapping
    | `Variation_selector
    | `Vertical_orientation
    | `White_space
    | `Word_break
    | `Xid_continue
    | `Xid_start
    | `KAccountingNumeric
    | `KAlternateHanYu
    | `KAlternateJEF
    | `KAlternateKangXi
    | `KAlternateMorohashi
    | `KBigFive
    | `KCCCII
    | `KCNS1986
    | `KCNS1992
    | `KCangjie
    | `KCantonese
    | `KCheungBauer
    | `KCheungBauerIndex
    | `KCihaiT
    | `KCompatibilityVariant
    | `KCowles
    | `KDaeJaweon
    | `KDefinition
    | `KEACC
    | `KFenn
    | `KFennIndex
    | `KFourCornerCode
    | `KFrequency
    | `KGB0
    | `KGB1
    | `KGB3
    | `KGB5
    | `KGB7
    | `KGB8
    | `KGSR
    | `KGradeLevel
    | `KHDZRadBreak
    | `KHKGlyph
    | `KHKSCS
    | `KHanYu
    | `KHangul
    | `KHanyuPinlu
    | `KHanyuPinyin
    | `KIBMJapan
    | `KIICore
    | `KIRGDaeJaweon
    | `KIRGDaiKanwaZiten
    | `KIRGHanyuDaZidian
    | `KIRGKangXi
    | `KIRG_GSource
    | `KIRG_HSource
    | `KIRG_JSource
    | `KIRG_KPSource
    | `KIRG_KSource
    | `KIRG_MSource
    | `KIRG_TSource
    | `KIRG_USource
    | `KIRG_VSource
    | `KJHJ
    | `KJIS0213
    | `KJa
    | `KJapaneseKun
    | `KJapaneseOn
    | `KJis0
    | `KJis1
    | `KKPS0
    | `KKPS1
    | `KKSC0
    | `KKSC1
    | `KKangXi
    | `KKarlgren
    | `KKorean
    | `KLau
    | `KMainlandTelegraph
    | `KMandarin
    | `KMatthews
    | `KMeyerWempe
    | `KMorohashi
    | `KNelson
    | `KOtherNumeric
    | `KPhonetic
    | `KPrimaryNumeric
    | `KPseudoGB1
    | `KRSAdobe_Japan1_6
    | `KRSJapanese
    | `KRSKanWa
    | `KRSKangXi
    | `KRSKorean
    | `KRSMerged
    | `KRSTUnicode
    | `KRSUnicode
    | `KReading
    | `KSBGY
    | `KSemanticVariant
    | `KSimplifiedVariant
    | `KSpecializedSemanticVariant
    | `KSrc_NushuDuben
    | `KTGT_MergedSrc
    | `KTaiwanTelegraph
    | `KTang
    | `KTotalStrokes
    | `KTraditionalVariant
    | `KVietnamese
    | `KWubi
    | `KXHC1983
    | `KXerox
    | `KZVariant
  ]

  type _ data =
    | Boolean : UCharTbl.Bool.t -> bool data
    | Variant : ('a array * UCharTbl.Bits.t) -> 'a data
    | Byte : UCharTbl.Char.t -> int data
    | Any : 'a UCharTbl.t -> 'a data

  let name_of = function
      `Age -> "age"
    | `Alphabetic -> "alphabetic"
    | `Ascii_hex_digit -> "ascii_hex_digit"
    | `Bidi_class -> "bidi_class"
    | `Bidi_control -> "bidi_control"
    | `Bidi_mirrored -> "bidi_mirrored"
    | `Bidi_mirroring_glyph -> "bidi_mirroring_glyph"
    | `Bidi_paired_bracket -> "bidi_paired_bracket"
    | `Bidi_paired_bracket_type -> "bidi_paired_bracket_type"
    | `Block -> "block"
    | `Canonical_combining_class -> "canonical_combining_class"
    | `Cased -> "cased"
    | `Case_folding -> "case_folding"
    | `Case_ignorable -> "case_ignorable"
    | `Changes_when_casefolded -> "changes_when_casefolded"
    | `Changes_when_casemapped -> "changes_when_casemapped"
    | `Changes_when_lowercased -> "changes_when_lowercased"
    | `Changes_when_nfkc_casefolded -> "changes_when_nfkc_casefolded"
    | `Changes_when_titlecased -> "changes_when_titlecased"
    | `Changes_when_uppercased -> "changes_when_uppercased"
    | `Composition_exclusion -> "composition_exclusion"
    | `Dash -> "dash"
    | `Decomposition_mapping -> "decomposition_mapping"
    | `Decomposition_type -> "decomposition_type"
    | `Default_ignorable_code_point -> "default_ignorable_code_point"
    | `Deprecated -> "deprecated"
    | `Diacritic -> "diacritic"
    | `East_asian_width -> "east_asian_width"
    | `Expands_on_nfc -> "expands_on_nfc"
    | `Expands_on_nfd -> "expands_on_nfd"
    | `Expands_on_nfkc -> "expands_on_nfkc"
    | `Expands_on_nfkd -> "expands_on_nfkd"
    | `Extender -> "extender"
    | `Fc_nfkc_closure -> "fc_nfkc_closure"
    | `Full_composition_exclusion -> "full_composition_exclusion"
    | `General_category -> "general_category"
    | `Grapheme_base -> "grapheme_base"
    | `Grapheme_cluster_break -> "grapheme_cluster_break"
    | `Grapheme_extend -> "grapheme_extend"
    | `Grapheme_link -> "grapheme_link"
    | `Hangul_syllable_type -> "hangul_syllable_type"
    | `Hex_digit -> "hex_digit"
    | `Hyphen -> "hyphen"
    | `Id_continue -> "id_continue"
    | `Id_start -> "id_start"
    | `Ideographic -> "ideographic"
    | `Ids_binary_operator -> "ids_binary_operator"
    | `Ids_trinary_operator -> "ids_trinary_operator"
    | `Indic_syllabic_category -> "indic_syllabic_category"
    | `Indic_matra_category -> "indic_matra_category"
    | `Indic_positional_category -> "indic_positional_category"
    | `Iso_comment -> "iso_comment"
    | `Jamo_short_name -> "jamo_short_name"
    | `Join_control -> "join_control"
    | `Joining_group -> "joining_group"
    | `Joining_type -> "joining_type"
    | `Line_break -> "line_break"
    | `Logical_order_exception -> "logical_order_exception"
    | `Lowercase -> "lowercase"
    | `Lowercase_mapping -> "lowercase_mapping"
    | `Math -> "math"
    | `Name -> "name"
    | `Name_alias -> "name_alias"
    | `Nfc_quick_check -> "nfc_quick_check"
    | `Nfd_quick_check -> "nfd_quick_check"
    | `Nfkc_quick_check -> "nfkc_quick_check"
    | `Nfkc_casefold -> "nfkc_casefold"
    | `Nfkd_quick_check -> "nfkd_quick_check"
    | `Noncharacter_code_point -> "noncharacter_code_point"
    | `Numeric_type -> "numeric_type"
    | `Numeric_value -> "numeric_value"
    | `Other_alphabetic -> "other_alphabetic"
    | `Other_default_ignorable_code_point -> "other_default_ignorable_code_point"
    | `Other_grapheme_extend -> "other_grapheme_extend"
    | `Other_id_continue -> "other_id_continue"
    | `Other_id_start -> "other_id_start"
    | `Other_lowercase -> "other_lowercase"
    | `Other_math -> "other_math"
    | `Other_uppercase -> "other_uppercase"
    | `Pattern_syntax -> "pattern_syntax"
    | `Pattern_white_space -> "pattern_white_space"
    | `Prepended_concatenation_mark -> "prepended_concatenation_mark"
    | `Quotation_mark -> "quotation_mark"
    | `Radical -> "radical"
    | `Regional_indicator -> "regional_indicator"
    | `Script -> "script"
    | `Script_extensions -> "script_extensions"
    | `Sentence_break -> "sentence_break"
    | `Simple_case_folding -> "simple_case_folding"
    | `Simple_lowercase_mapping -> "simple_lowercase_mapping"
    | `Simple_titlecase_mapping -> "simple_titlecase_mapping"
    | `Simple_uppercase_mapping -> "simple_uppercase_mapping"
    | `Soft_dotted -> "soft_dotted"
    | `Sterm -> "sterm"
    | `Terminal_punctuation -> "terminal_punctuation"
    | `Titlecase_mapping -> "titlecase_mapping"
    | `Uax_42_element -> "uax_42_element"
    | `Unicode_1_name -> "unicode_1_name"
    | `Unified_ideograph -> "unified_ideograph"
    | `Uppercase -> "uppercase"
    | `Uppercase_mapping -> "uppercase_mapping"
    | `Variation_selector -> "variation_selector"
    | `Vertical_orientation -> "vertical_orientation"
    | `White_space -> "white_space"
    | `Word_break -> "word_break"
    | `Xid_continue -> "xid_continue"
    | `Xid_start -> "xid_start"
    | `KAccountingNumeric -> "kAccountingNumeric"
    | `KAlternateHanYu -> "kAlternateHanYu"
    | `KAlternateJEF -> "kAlternateJEF"
    | `KAlternateKangXi -> "kAlternateKangXi"
    | `KAlternateMorohashi -> "kAlternateMorohashi"
    | `KBigFive -> "kBigFive"
    | `KCCCII -> "kCCCII"
    | `KCNS1986 -> "kCNS1986"
    | `KCNS1992 -> "kCNS1992"
    | `KCangjie -> "kCangjie"
    | `KCantonese -> "kCantonese"
    | `KCheungBauer -> "kCheungBauer"
    | `KCheungBauerIndex -> "kCheungBauerIndex"
    | `KCihaiT -> "kCihaiT"
    | `KCompatibilityVariant -> "kCompatibilityVariant"
    | `KCowles -> "kCowles"
    | `KDaeJaweon -> "kDaeJaweon"
    | `KDefinition -> "kDefinition"
    | `KEACC -> "kEACC"
    | `KFenn -> "kFenn"
    | `KFennIndex -> "kFennIndex"
    | `KFourCornerCode -> "kFourCornerCode"
    | `KFrequency -> "kFrequency"
    | `KGB0 -> "kGB0"
    | `KGB1 -> "kGB1"
    | `KGB3 -> "kGB3"
    | `KGB5 -> "kGB5"
    | `KGB7 -> "kGB7"
    | `KGB8 -> "kGB8"
    | `KGSR -> "kGSR"
    | `KGradeLevel -> "kGradeLevel"
    | `KHDZRadBreak -> "kHDZRadBreak"
    | `KHKGlyph -> "kHKGlyph"
    | `KHKSCS -> "kHKSCS"
    | `KHanYu -> "kHanYu"
    | `KHangul -> "kHangul"
    | `KHanyuPinlu -> "kHanyuPinlu"
    | `KHanyuPinyin -> "kHanyuPinyin"
    | `KIBMJapan -> "kIBMJapan"
    | `KIICore -> "kIICore"
    | `KIRGDaeJaweon -> "kIRGDaeJaweon"
    | `KIRGDaiKanwaZiten -> "kIRGDaiKanwaZiten"
    | `KIRGHanyuDaZidian -> "kIRGHanyuDaZidian"
    | `KIRGKangXi -> "kIRGKangXi"
    | `KIRG_GSource -> "kIRG_GSource"
    | `KIRG_HSource -> "kIRG_HSource"
    | `KIRG_JSource -> "kIRG_JSource"
    | `KIRG_KPSource -> "kIRG_KPSource"
    | `KIRG_KSource -> "kIRG_KSource"
    | `KIRG_MSource -> "kIRG_MSource"
    | `KIRG_TSource -> "kIRG_TSource"
    | `KIRG_USource -> "kIRG_USource"
    | `KIRG_VSource -> "kIRG_VSource"
    | `KJHJ -> "kJHJ"
    | `KJIS0213 -> "kJIS0213"
    | `KJa -> "kJa"
    | `KJapaneseKun -> "kJapaneseKun"
    | `KJapaneseOn -> "kJapaneseOn"
    | `KJis0 -> "kJis0"
    | `KJis1 -> "kJis1"
    | `KKPS0 -> "kKPS0"
    | `KKPS1 -> "kKPS1"
    | `KKSC0 -> "kKSC0"
    | `KKSC1 -> "kKSC1"
    | `KKangXi -> "kKangXi"
    | `KKarlgren -> "kKarlgren"
    | `KKorean -> "kKorean"
    | `KLau -> "kLau"
    | `KMainlandTelegraph -> "kMainlandTelegraph"
    | `KMandarin -> "kMandarin"
    | `KMatthews -> "kMatthews"
    | `KMeyerWempe -> "kMeyerWempe"
    | `KMorohashi -> "kMorohashi"
    | `KNelson -> "kNelson"
    | `KOtherNumeric -> "kOtherNumeric"
    | `KPhonetic -> "kPhonetic"
    | `KPrimaryNumeric -> "kPrimaryNumeric"
    | `KPseudoGB1 -> "kPseudoGB1"
    | `KRSAdobe_Japan1_6 -> "kRSAdobe_Japan1_6"
    | `KRSJapanese -> "kRSJapanese"
    | `KRSKanWa -> "kRSKanWa"
    | `KRSKangXi -> "kRSKangXi"
    | `KRSKorean -> "kRSKorean"
    | `KRSMerged -> "kRSMerged"
    | `KRSTUnicode -> "kRSTUnicode"
    | `KRSUnicode -> "kRSUnicode"
    | `KReading -> "kReading"
    | `KSBGY -> "kSBGY"
    | `KSemanticVariant -> "kSemanticVariant"
    | `KSimplifiedVariant -> "kSimplifiedVariant"
    | `KSpecializedSemanticVariant -> "kSpecializedSemanticVariant"
    | `KSrc_NushuDuben -> "kSrc_NushuDuben"
    | `KTGT_MergedSrc -> "kTGT_MergedSrc"
    | `KTaiwanTelegraph -> "kTaiwanTelegraph"
    | `KTang -> "kTang"
    | `KTotalStrokes -> "kTotalStrokes"
    | `KTraditionalVariant -> "kTraditionalVariant"
    | `KVietnamese -> "kVietnamese"
    | `KWubi -> "kWubi"
    | `KXHC1983 -> "kXHC1983"
    | `KXerox -> "kXerox"
    | `KZVariant -> "kZVariant"

  let get_data :  'a prop -> 'a data  =
    function p -> UData.read_data (name_of p)

  let get_value : type a. a data -> UChar.t -> a = function
      Boolean tbl -> UCharTbl.Bool.get tbl
    | Variant (m, tbl) -> (function u -> Array.unsafe_get m (UCharTbl.Bits.get tbl u)
      )
    | Byte tbl -> (function u -> Char.code (UCharTbl.Char.get tbl u))
    | Any tbl -> UCharTbl.get tbl

  let get_set p = UData.read_data ((name_of p) ^ "-set")
  let get_map p = UData.read_data ((name_of p) ^ "-map")

  let older v1 v2 =
    match v1, v2 with
      `Unassigned, `Unassigned -> true
    | `Version (_, _), `Unassigned -> true
    | `Unassigned, `Version (_, _) -> false
    | `Version (v1, v2), `Version (v1', v2') ->
      if v1 < v1' || (v1 = v1' && v2 <= v2') then true else false

  let age_prop = `Age
  let alphabetic_prop = `Alphabetic
  let ascii_hex_digit_prop = `Ascii_hex_digit
  let bidi_class_prop = `Bidi_class
  let bidi_control_prop = `Bidi_control
  let bidi_mirrored_prop = `Bidi_mirrored
  let bidi_mirroring_glyph_prop = `Bidi_mirroring_glyph
  let bidi_paired_bracket_prop = `Bidi_paired_bracket
  let bidi_paired_bracket_type_prop = `Bidi_paired_bracket_type
  let block_prop = `Block
  let canonical_combining_class_prop = `Canonical_combining_class
  let cased_prop = `Cased
  let case_folding_prop = `Case_folding
  let case_ignorable_prop = `Case_ignorable
  let changes_when_casefolded_prop = `Changes_when_casefolded
  let changes_when_casemapped_prop = `Changes_when_casemapped
  let changes_when_lowercased_prop = `Changes_when_lowercased
  let changes_when_nfkc_casefolded_prop = `Changes_when_nfkc_casefolded
  let changes_when_titlecased_prop = `Changes_when_titlecased
  let changes_when_uppercased_prop = `Changes_when_uppercased
  let composition_exclusion_prop = `Composition_exclusion
  let dash_prop = `Dash
  let decomposition_mapping_prop = `Decomposition_mapping
  let decomposition_type_prop = `Decomposition_type
  let default_ignorable_code_point_prop = `Default_ignorable_code_point
  let deprecated_prop = `Deprecated
  let diacritic_prop = `Diacritic
  let east_asian_width_prop = `East_asian_width
  let expands_on_nfc_prop = `Expands_on_nfc
  let expands_on_nfd_prop = `Expands_on_nfd
  let expands_on_nfkc_prop = `Expands_on_nfkc
  let expands_on_nfkd_prop = `Expands_on_nfkd
  let extender_prop = `Extender
  let fc_nfkc_closure_prop = `Fc_nfkc_closure
  let full_composition_exclusion_prop = `Full_composition_exclusion
  let general_category_prop = `General_category
  let grapheme_base_prop = `Grapheme_base
  let grapheme_cluster_break_prop = `Grapheme_cluster_break
  let grapheme_extend_prop = `Grapheme_extend
  let grapheme_link_prop = `Grapheme_link
  let hangul_syllable_type_prop = `Hangul_syllable_type
  let hex_digit_prop = `Hex_digit
  let hyphen_prop = `Hyphen
  let id_continue_prop = `Id_continue
  let id_start_prop = `Id_start
  let ideographic_prop = `Ideographic
  let ids_binary_operator_prop = `Ids_binary_operator
  let ids_trinary_operator_prop = `Ids_trinary_operator
  let indic_syllabic_category_prop = `Indic_syllabic_category
  let indic_matra_category_prop = `Indic_matra_category
  let indic_positional_category_prop = `Indic_positional_category
  let iso_comment_prop = `Iso_comment
  let jamo_short_name_prop = `Jamo_short_name
  let join_control_prop = `Join_control
  let joining_group_prop = `Joining_group
  let joining_type_prop = `Joining_type
  let line_break_prop = `Line_break
  let logical_order_exception_prop = `Logical_order_exception
  let lowercase_prop = `Lowercase
  let lowercase_mapping_prop = `Lowercase_mapping
  let math_prop = `Math
  let name_prop = `Name
  let name_alias_prop = `Name_alias
  let nfc_quick_check_prop = `Nfc_quick_check
  let nfd_quick_check_prop = `Nfd_quick_check
  let nfkc_quick_check_prop = `Nfkc_quick_check
  let nfkc_casefold_prop = `Nfkc_casefold
  let nfkd_quick_check_prop = `Nfkd_quick_check
  let noncharacter_code_point_prop = `Noncharacter_code_point
  let numeric_type_prop = `Numeric_type
  let numeric_value_prop = `Numeric_value
  let other_alphabetic_prop = `Other_alphabetic
  let other_default_ignorable_code_point_prop = `Other_default_ignorable_code_point
  let other_grapheme_extend_prop = `Other_grapheme_extend
  let other_id_continue_prop = `Other_id_continue
  let other_id_start_prop = `Other_id_start
  let other_lowercase_prop = `Other_lowercase
  let other_math_prop = `Other_math
  let other_uppercase_prop = `Other_uppercase
  let pattern_syntax_prop = `Pattern_syntax
  let pattern_white_space_prop = `Pattern_white_space
  let prepended_concatenation_mark_prop = `Prepended_concatenation_mark
  let quotation_mark_prop = `Quotation_mark
  let radical_prop = `Radical
  let regional_indicator_prop = `Regional_indicator
  let script_prop = `Script
  let script_extensions_prop = `Script_extensions
  let sentence_break_prop = `Sentence_break
  let simple_case_folding_prop = `Simple_case_folding
  let simple_lowercase_mapping_prop = `Simple_lowercase_mapping
  let simple_titlecase_mapping_prop = `Simple_titlecase_mapping
  let simple_uppercase_mapping_prop = `Simple_uppercase_mapping
  let soft_dotted_prop = `Soft_dotted
  let sterm_prop = `Sterm
  let terminal_punctuation_prop = `Terminal_punctuation
  let titlecase_mapping_prop = `Titlecase_mapping
  let uax_42_element_prop = `Uax_42_element
  let unicode_1_name_prop = `Unicode_1_name
  let unified_ideograph_prop = `Unified_ideograph
  let uppercase_prop = `Uppercase
  let uppercase_mapping_prop = `Uppercase_mapping
  let variation_selector_prop = `Variation_selector
  let vertical_orientation_prop = `Vertical_orientation
  let white_space_prop = `White_space
  let word_break_prop = `Word_break
  let xid_continue_prop = `Xid_continue
  let xid_start_prop = `Xid_start
  let kAccountingNumeric_prop = `KAccountingNumeric
  let kAlternateHanYu_prop = `KAlternateHanYu
  let kAlternateJEF_prop = `KAlternateJEF
  let kAlternateKangXi_prop = `KAlternateKangXi
  let kAlternateMorohashi_prop = `KAlternateMorohashi
  let kBigFive_prop = `KBigFive
  let kCCCII_prop = `KCCCII
  let kCNS1986_prop = `KCNS1986
  let kCNS1992_prop = `KCNS1992
  let kCangjie_prop = `KCangjie
  let kCantonese_prop = `KCantonese
  let kCheungBauer_prop = `KCheungBauer
  let kCheungBauerIndex_prop = `KCheungBauerIndex
  let kCihaiT_prop = `KCihaiT
  let kCompatibilityVariant_prop = `KCompatibilityVariant
  let kCowles_prop = `KCowles
  let kDaeJaweon_prop = `KDaeJaweon
  let kDefinition_prop = `KDefinition
  let kEACC_prop = `KEACC
  let kFenn_prop = `KFenn
  let kFennIndex_prop = `KFennIndex
  let kFourCornerCode_prop = `KFourCornerCode
  let kFrequency_prop = `KFrequency
  let kGB0_prop = `KGB0
  let kGB1_prop = `KGB1
  let kGB3_prop = `KGB3
  let kGB5_prop = `KGB5
  let kGB7_prop = `KGB7
  let kGB8_prop = `KGB8
  let kGSR_prop = `KGSR
  let kGradeLevel_prop = `KGradeLevel
  let kHDZRadBreak_prop = `KHDZRadBreak
  let kHKGlyph_prop = `KHKGlyph
  let kHKSCS_prop = `KHKSCS
  let kHanYu_prop = `KHanYu
  let kHangul_prop = `KHangul
  let kHanyuPinlu_prop = `KHanyuPinlu
  let kHanyuPinyin_prop = `KHanyuPinyin
  let kIBMJapan_prop = `KIBMJapan
  let kIICore_prop = `KIICore
  let kIRGDaeJaweon_prop = `KIRGDaeJaweon
  let kIRGDaiKanwaZiten_prop = `KIRGDaiKanwaZiten
  let kIRGHanyuDaZidian_prop = `KIRGHanyuDaZidian
  let kIRGKangXi_prop = `KIRGKangXi
  let kIRG_GSource_prop = `KIRG_GSource
  let kIRG_HSource_prop = `KIRG_HSource
  let kIRG_JSource_prop = `KIRG_JSource
  let kIRG_KPSource_prop = `KIRG_KPSource
  let kIRG_KSource_prop = `KIRG_KSource
  let kIRG_MSource_prop = `KIRG_MSource
  let kIRG_TSource_prop = `KIRG_TSource
  let kIRG_USource_prop = `KIRG_USource
  let kIRG_VSource_prop = `KIRG_VSource
  let kJHJ_prop = `KJHJ
  let kJIS0213_prop = `KJIS0213
  let kJa_prop = `KJa
  let kJapaneseKun_prop = `KJapaneseKun
  let kJapaneseOn_prop = `KJapaneseOn
  let kJis0_prop = `KJis0
  let kJis1_prop = `KJis1
  let kKPS0_prop = `KKPS0
  let kKPS1_prop = `KKPS1
  let kKSC0_prop = `KKSC0
  let kKSC1_prop = `KKSC1
  let kKangXi_prop = `KKangXi
  let kKarlgren_prop = `KKarlgren
  let kKorean_prop = `KKorean
  let kLau_prop = `KLau
  let kMainlandTelegraph_prop = `KMainlandTelegraph
  let kMandarin_prop = `KMandarin
  let kMatthews_prop = `KMatthews
  let kMeyerWempe_prop = `KMeyerWempe
  let kMorohashi_prop = `KMorohashi
  let kNelson_prop = `KNelson
  let kOtherNumeric_prop = `KOtherNumeric
  let kPhonetic_prop = `KPhonetic
  let kPrimaryNumeric_prop = `KPrimaryNumeric
  let kPseudoGB1_prop = `KPseudoGB1
  let kRSAdobe_Japan1_6_prop = `KRSAdobe_Japan1_6
  let kRSJapanese_prop = `KRSJapanese
  let kRSKanWa_prop = `KRSKanWa
  let kRSKangXi_prop = `KRSKangXi
  let kRSKorean_prop = `KRSKorean
  let kRSMerged_prop = `KRSMerged
  let kRSTUnicode_prop = `KRSTUnicode
  let kRSUnicode_prop = `KRSUnicode
  let kReading_prop = `KReading
  let kSBGY_prop = `KSBGY
  let kSemanticVariant_prop = `KSemanticVariant
  let kSimplifiedVariant_prop = `KSimplifiedVariant
  let kSpecializedSemanticVariant_prop = `KSpecializedSemanticVariant
  let kSrc_NushuDuben_prop = `KSrc_NushuDuben
  let kTGT_MergedSrc_prop = `KTGT_MergedSrc
  let kTaiwanTelegraph_prop = `KTaiwanTelegraph
  let kTang_prop = `KTang
  let kTotalStrokes_prop = `KTotalStrokes
  let kTraditionalVariant_prop = `KTraditionalVariant
  let kVietnamese_prop = `KVietnamese
  let kWubi_prop = `KWubi
  let kXHC1983_prop = `KXHC1983
  let kXerox_prop = `KXerox
  let kZVariant_prop = `KZVariant

  (* Relic of old code.  To be removed *)
  (* General category *)

  let general_category_tbl : UCharTbl.Bits.t =
    UData.read_data "general_category"

  let general_category u =
    match UCharTbl.Bits.get general_category_tbl u with
      0 ->
      let n = UChar.uint_code u in
      if n >= 0x0f0000 && n <= 0x100000 then `Co else
      if n >= 0xe00000 && n <= 0xff0000 then `Co else
      if n >= 0x60000000 && n <= 0x7f000000 then `Co else `Cn
    | x -> UData.cat_of_num x

  let load_general_category_map () =
    UData.read_data "general_category_map"

  (* character property *)

  type character_property_type =
    [ `Math				(*Derived Core Properties*)
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
    | `Bidi_Control			(*Extended Properties*)
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

  let name_of_property p =
    match p with
      `Math -> "Math"
    | `Alphabetic -> "Alphabetic"
    | `Lowercase -> "Lowercase"
    | `Uppercase -> "Uppercase"
    | `ID_Start -> "ID_Start"
    | `ID_Continue -> "ID_Continue"
    | `XID_Start -> "XID_Start"
    | `XID_Continue -> "XID_Continue"
    | `Default_Ignorable_Code_Point -> "Default_Ignorable_Code_Point"
    | `Grapheme_Extend -> "Grapheme_Extend"
    | `Grapheme_Base -> "Grapheme_Base"
    | `Bidi_Control -> "Bidi_Control"
    | `White_Space -> "White_Space"
    | `Hyphen -> "Hyphen"
    | `Quotation_Mark -> "Quotation_Mark"
    | `Terminal_Punctuation -> "Terminal_Punctuation"
    | `Other_Math -> "Other_Math"
    | `Hex_Digit -> "Hex_Digit"
    | `Ascii_Hex_Digit -> "Ascii_Hex_Digit"
    | `Other_Alphabetic -> "Other_Alphabetic"
    | `Ideographic -> "Ideographic"
    | `Diacritic -> "Diacritic"
    | `Extender -> "Extender"
    | `Other_Lowercase -> "Other_Lowercase"
    | `Other_Uppercase -> "Other_Uppercase"
    | `Noncharacter_Code_Point -> "Noncharacter_Code_Point"
    | `Other_Grapheme_Extend -> "Other_Grapheme_Extend"
    | `Grapheme_Link -> "Grapheme_Link"
    | `IDS_Binary_Operator -> "IDS_Binary_Operator"
    | `IDS_Trinary_Operator -> "IDS_Trinary_Operator"
    | `Radical -> "Radical"
    | `Unified_Ideograph -> "Unified_Ideograph"
    | `Other_default_Ignorable_Code_Point -> "Other_default_Ignorable_Code_Point"
    | `Deprecated -> "Deprecated"
    | `Soft_Dotted -> "Soft_Dotted"
    | `Logical_Order_Exception -> "Logical_Order_Exception"

  let property_of_name : string -> character_property_type = function
      "Math" -> `Math
    | "Alphabetic"  -> `Alphabetic
    | "Lowercase" -> `Lowercase
    | "Uppercase" -> `Uppercase
    | "ID_Start" -> `ID_Start
    | "ID_Continue" -> `ID_Continue
    | "XID_Start" -> `XID_Start
    | "XID_Continue" -> `XID_Continue
    | "Default_Ignorable_Code_Point" -> `Default_Ignorable_Code_Point
    | "Grapheme_Extend" -> `Grapheme_Extend
    | "Grapheme_Base" -> `Grapheme_Base
    | "Bidi_Control" -> `Bidi_Control
    | "White_Space" -> `White_Space
    | "Hyphen" -> `Hyphen
    | "Quotation_Mark" -> `Quotation_Mark
    | "Terminal_Punctuation" -> `Terminal_Punctuation
    | "Other_Math" -> `Other_Math
    | "Hex_Digit" -> `Hex_Digit
    | "Ascii_Hex_Digit" -> `Ascii_Hex_Digit
    | "Other_Alphabetic" -> `Other_Alphabetic
    | "Ideographic" -> `Ideographic
    | "Diacritic" -> `Diacritic
    | "Extender" -> `Extender
    | "Other_Lowercase" -> `Other_Lowercase
    | "Other_Uppercase" -> `Other_Uppercase
    | "Noncharacter_Code_Point" -> `Noncharacter_Code_Point
    | "Other_Grapheme_Extend" -> `Other_Grapheme_Extend
    | "Grapheme_Link" -> `Grapheme_Link
    | "IDS_Binary_Operator" -> `IDS_Binary_Operator
    | "IDS_Trinary_Operator" -> `IDS_Trinary_Operator
    | "Radical" -> `Radical
    | "Unified_Ideograph" -> `Unified_Ideograph
    | "Other_default_Ignorable_Code_Point" -> `Other_default_Ignorable_Code_Point
    | "Deprecated" -> `Deprecated
    | "Soft_Dotted" -> `Soft_Dotted
    | "Logical_Order_Exception" -> `Logical_Order_Exception
    | _ -> raise Not_found

  let loaded_props = Hashtbl.create 0

  let load_property_tbl p =
    try
      let b = Hashtbl.find loaded_props p in
      match Weak.get b 0 with
        None ->
        Hashtbl.remove loaded_props p;
        raise Not_found
      | Some x -> x
    with Not_found ->
      let tbl = UData.read_data (name_of_property p) in
      let b = Weak.create 1 in
      Weak.set b 0 (Some tbl);
      Hashtbl.add loaded_props p b;
      tbl

  let load_property_tbl_by_name s =
    load_property_tbl (property_of_name s)

  let loaded_prop_sets = Hashtbl.create 0

  let load_property_set p =
    try
      let b = Hashtbl.find loaded_prop_sets p in
      match Weak.get b 0 with
        None ->
        Hashtbl.remove loaded_prop_sets p;
        raise Not_found
      | Some x -> x
    with Not_found ->
      let tbl = UData.read_data  ((name_of_property p) ^ "_set") in
      let b = Weak.create 1 in
      Weak.set b 0 (Some tbl);
      Hashtbl.add loaded_prop_sets p b;
      tbl

  let load_property_set_by_name s =
    load_property_set (property_of_name s)

  (* Scripts *)

  let script_tbl : UCharTbl.Bits.t = UData.read_data "scripts"

  let script u = UData.script_of_num (UCharTbl.Bits.get script_tbl u)
  let load_script_map () = UData.read_data "scripts_map"

  type version_type =
    [ `Nc
    | `v1_0
    | `v1_1
    | `v2_0
    | `v2_1
    | `v3_0
    | `v3_1
    | `v3_2 ]

  let version_of_char = function
    | '\x10' -> `v1_0
    | '\x11' -> `v1_1
    | '\x20' -> `v2_0
    | '\x21' -> `v2_1
    | '\x30' -> `v3_0
    | '\x31' -> `v3_1
    | '\x32' -> `v3_2
    | '\xfe' -> `Nc
    | i -> failwith (Printf.sprintf "version_of_char, unknown version v%x" (Char.code i))

  let version_to_char = function
    | `v1_0 -> '\x10'
    | `v1_1 -> '\x11'
    | `v2_0 -> '\x20'
    | `v2_1 -> '\x21'
    | `v3_0 -> '\x30'
    | `v3_1 -> '\x31'
    | `v3_2 -> '\x32'
    | `Nc   -> '\xfe'

  let age_tbl : UCharTbl.Char.t = UData.read_data  "age"

  let age u = version_of_char (UCharTbl.Char.get age_tbl u)
  (* Casing *)

  let cache = Weak.create 3

  let load_to_lower1_tbl () =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "to_lower1" in
      Weak.set cache 0 (Some t);
      t

  let load_to_upper1_tbl () =
    match Weak.get cache 1 with
      Some t -> t
    | None ->
      let t = UData.read_data  "to_upper1" in
      Weak.set cache 1 (Some t);
      t

  let load_to_title1_tbl () =
    match Weak.get cache 2 with
      Some t -> t
    | None ->
      let t = UData.read_data  "to_title1" in
      Weak.set cache 2 (Some t);
      t

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

  let cache = Weak.create 1

  let load_conditional_casing_tbl () : special_casing_property list UCharTbl.t =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "special_casing" in
      Weak.set cache 0 (Some t);
      t

  let cache = Weak.create 1

  let load_casefolding_tbl () =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "case_folding" in
      Weak.set cache 0 (Some t);
      t

  (* Combined class *)

  let combined_class_tbl : UCharTbl.Char.t =
    UData.read_data  "combined_class"

  let combined_class u = Char.code (UCharTbl.Char.get combined_class_tbl u)

  (* Decomposition *)

  let cache = Weak.create 1

  let load_decomposition_tbl () =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "decomposition" in
      Weak.set cache 0 (Some t);
      t

  (* Composition *)

  let cache = Weak.create 1

  let load_composition_tbl () =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "composition" in
      Weak.set cache 0 (Some t);
      t

  let cache = Weak.create 1

  let load_composition_exclusion_tbl () =
    match Weak.get cache 0 with
      Some t -> t
    | None ->
      let t = UData.read_data  "composition_exclusion" in
      Weak.set cache 0 (Some t);
      t

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

end
