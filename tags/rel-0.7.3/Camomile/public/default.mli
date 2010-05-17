(** modules with default configuration.  Almost compatible to Camomile 0.6.x *)

module Camomile : Main.Type with
      module ISet = ISet and
      module IMap = IMap and
      module XArray = XArray and 
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
      module UTF8 = UTF8 and
      module UTF16 = UTF16 and
      module UCS4 = UCS4
