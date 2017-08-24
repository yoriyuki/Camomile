module Camomile = CamomileLibrary.Make (struct
    let pwd = Filename.dirname (Sys.argv.(0))

    let dir s =
      Filename.concat (Filename.concat pwd "../../Camomile") s
    let localedir = dir "locales"
    let unimapdir = dir "mappings"
    let charmapdir = dir "charmaps"
    let datadir = dir "database"
  end)
