(** modules with default configuration.  Almost compatible to Camomile 0.6.x *)

module Camomile : CamomileLibrary.Type with
      module OOChannel = CamomileLibrary.OOChannel and
      module UChar = CamomileLibrary.UChar and
      module USet = CamomileLibrary.USet and
      module UMap = CamomileLibrary.UMap and
      module UCharTbl = CamomileLibrary.UCharTbl and
      module UnicodeString = CamomileLibrary.UnicodeString and
      module UText = CamomileLibrary.UText and
      module XString = CamomileLibrary.XString and
      module SubText = CamomileLibrary.SubText and
      module ULine = CamomileLibrary.ULine and
      module UTF8 = CamomileLibrary.UTF8 and
      module UTF16 = CamomileLibrary.UTF16 and
      module UCS4 = CamomileLibrary.UCS4 and
      module UCharInfo = CamomileLibrary.UCharInfo.Make(
	CamomileLibrary.CamomileDefaultConfig) and
      module UNF.Make = CamomileLibrary.UNF.Make(
	CamomileLibrary.CamomileDefaultConfig) and
      module UCol.Make = CamomileLibrary.UCol.Make(
	CamomileLibrary.CamomileDefaultConfig) and
      module UReStr = CamomileLibrary.UReStr.Configure(
	CamomileLibrary.CamomileDefaultConfig) and
      module StringPrep.Make = CamomileLibrary.StringPrep.Make(
	CamomileLibrary.CamomileDefaultConfig)
      
