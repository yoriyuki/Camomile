(* Copyright 2010, Sylvain Le Gal, Yamagata Yoriyuki, distributed with LGPL *)
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

(** Directory of compiled Unicode data *)
val datadir : string 

(** Directory of compiled character mapping tables a la ISO *)
val charmapdir : string 

(** Directory of camomile-style compiled character mapping table *)
val unimapdir : string 

(** Directory of compiled locale data *)
val localedir : string
