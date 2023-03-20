open CamomileLib

module Private = struct
  module AvlTree = AvlTree
  module Bitsvect = Bitsvect
  module Bytesvect = Bytesvect
  module Byte_labeled_dag = Byte_labeled_dag
  module Charmap = Charmap
  module Database = Database
  module Hangul = Hangul
  module IMap = IMap
  module ISet = ISet
  module StringPrep_data = StringPrep_data
  module Tbl31 = Tbl31
  module UReStrLexer = UReStrLexer
  module UReStrParser = UReStrParser
  module UReStrParserType = UReStrParserType
  module Unidata = Unidata
  module Unimap = Unimap
  module XArray = XArray
end

module DefaultConfig = Config.Default

module type Type = sig
  module OOChannel : module type of OOChannel
  module USet : module type of USet
  module UChar : module type of UChar
  module UMap : module type of UMap
  module UCharTbl : module type of UCharTbl
  module UnicodeString : module type of UnicodeString
  module UText : module type of UText
  module XString : module type of XString
  module SubText : module type of SubText
  module ULine : module type of ULine
  module Locale : module type of Locale
  module CharEncoding : CharEncoding.Interface
  module UTF8 : module type of UTF8
  module UTF16 : module type of UTF16
  module UCS4 : module type of UCS4
  module UPervasives : module type of UPervasives
  module URe : module type of URe
  module UCharInfo : UCharInfo.Type

  module UNF : sig
    module type Type = UNF.Type

    module Make (Text : UnicodeString.Type) :
      Type with type text = Text.t and type index = Text.index
  end

  module UCol : sig
    (** How variables are handled *)
    type variable_option =
      [ `Blanked | `Non_ignorable | `Shifted | `Shift_Trimmed ]

    (** Strength of comparison.  For European languages, each strength
        roughly means as
        `Primary : Ignore accents and case
        `Secondary : Ignore case but accents are counted in.
        `Tertiary : Accents and case are counted in.
        For the case of `Shifted, `Shift_Trimmed, there is the fourth strength.
        `Quaternary : Variables such as - (hyphen) are counted in. *)
    type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]

    module type Type = UCol.Type

    module Make (Text : UnicodeString.Type) :
      Type with type text = Text.t and type index = Text.index
  end

  module CaseMap : sig
    module type Type = CaseMap.Type

    module Make (Text : UnicodeString.Type) : Type with type text = Text.t
  end

  module UReStr : UReStr.Interface

  module StringPrep : sig
    module type Type = StringPrep.Type

    module Make (Text : UnicodeString.Type) : Type with type text = Text.t
  end
end

module Make (Config : Config.Type) = struct
  module OOChannel = OOChannel
  module UChar = UChar
  module USet = USet
  module UMap = UMap
  module UCharTbl = UCharTbl
  module UnicodeString = UnicodeString
  module UText = UText
  module XString = XString
  module SubText = SubText
  module ULine = ULine
  module Locale = Locale
  module CharEncoding = CharEncoding.Configure (Config)
  module UTF8 = UTF8
  module UTF16 = UTF16
  module UCS4 = UCS4
  module UPervasives = UPervasives
  module URe = URe
  module UCharInfo = UCharInfo.Make (Config)

  module UNF = struct
    module type Type = UNF.Type

    module Make = UNF.Make (Config)
  end

  module UCol = struct
    type variable_option =
      [ `Blanked | `Non_ignorable | `Shifted | `Shift_Trimmed ]

    type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]

    module type Type = UCol.Type

    module Make = UCol.Make (Config)
  end

  module CaseMap = struct
    module type Type = CaseMap.Type

    module Make = CaseMap.Make (Config)
  end

  module UReStr = UReStr.Configure (Config)

  module StringPrep = struct
    module type Type = StringPrep.Type

    module Make = StringPrep.Make (Config)
  end
end

include Make (Config.Default)
