(* Copyright 2010, Sylvain Le Gal, Yamagata Yoriyuki, distributed with LGPL *)

let camomile_dir_var = 
  "CAMOMILE_DIR"

let camomile_dir () =
  Sys.getenv camomile_dir_var

let find_dir var dn dflt =
  try 
    let f = 
      List.find 
        (fun f ->
          try 
            let dn = f () in 
            Sys.is_directory dn 
          with _ ->
            false)
        [
         (fun () -> Sys.getenv var);
         (fun () -> Filename.concat (camomile_dir ()) dn);
         (fun () -> dflt);
       ]
    in
    f ()
  with Not_found ->
    failwith 
      (Printf.sprintf 
         "Cannot find camomile %s directory, usually located here: '%s'. \
         Use environment variable %s or %s to locate it precisely."
  dn dflt
  camomile_dir_var var)

  module Default = CamomileDefaultConfig

let datadir = 
  find_dir 
    "CAMOMILE_DATADIR" 
    "database"
    Default.datadir
    
let localedir = 
  find_dir
    "CAMOMILE_LOCALEDIR"
    "locales"
    Default.localedir
    
let charmapdir =
  find_dir
    "CAMOMILE_CHARMAPDIR"
    "charmaps"
    Default.charmapdir
    
let unimapdir =
  find_dir
    "CAMOMILE_UNIMAPDIR"
    "mappings"
    Default.unimapdir
