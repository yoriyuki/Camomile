(* Copyright 2010, Sylvain Le Gal, Yamagata Yoriyuki, distributed with LGPL *)
(** modules configured by environmental variables.  *)

open CamomileLibrary

(** Configuration by environmental variables.  This module searchs the*)
(** locations given by environmental variables, and returns "likely"*)
(** location of them.  You can specifies the*)
(** root directory of all camomile datafiles by CAMOMILE_DIR. You can*)
(** set CAMOMILE_DATADIR if you put contents of "database" to the*)
(** dirctory other than $CAMOMILE_DIR/database.  In the same way, you*)
(** can specify the location of charmap files by CAMOMILE_CHARMAPDIR,*)
(** the location of mappings used in the same East Asian codeing by*)
(** CAMOILE_UNIMAPDIR, and the location of locale data by*)
(** CAMOMILE_LOCALEDIR.  *) 
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
