(* configuration for tools *)

let datadir = 
  try Sys.getenv "CAMOMILE_DATADIR" with Not_found -> 
    CamomileDefaultConfig.datadir

let localedir = 
  try Sys.getenv "CAMOMILE_LOCALEDIR" with Not_found -> 
    CamomileDefaultConfig.localedir

let charmapdir = 
  try Sys.getenv "CAMOMILE_CHARMAPDIR" with Not_found -> 
    CamomileDefaultConfig.charmapdir

let unimapdir =
  try Sys.getenv "CAMOMILE_UNIMAPDIR" with Not_found -> 
    CamomileDefaultConfig.unimapdir
