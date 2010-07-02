(** modules with default configuration.  Almost compatible to Camomile 0.6.x *)

open CamomileLibrary

module Config : ConfigInt.Type

module Camomile : CamomileLibrary.Type with
      module OOChannel = OOChannel and
      module UChar = UChar and
      module USet = USet and
      module UMap = UMap and
      module UCharTbl = UCharTbl and
      module UnicodeString = UnicodeString and
      module UText = UText and
      module XString = XString and
      module SubText = SubText and
      module ULine = ULine and
      module Locale = Locale and
      module CharEncoding = CharEncoding.Configure(Config) and
      module UTF8 = UTF8 and
      module UTF16 = UTF16 and
      module UCS4 = UCS4 and
      module UPervasives = UPervasives and
      module URe = URe and
      module UCharInfo = UCharInfo.Make(Config) and
      module UNF.Make = UNF.Make(Config) and
      module UCol.Make = UCol.Make(Config) and
      module CaseMap.Make = CaseMap.Make(Config) and
      module UReStr = UReStr.Configure(Config) and
      module StringPrep.Make = StringPrep.Make(Config)
