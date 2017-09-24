module Camomile = CamomileLibrary.Make (struct
    let pwd = Unix.getcwd ()

    let dir =
      Filename.concat (Filename.concat pwd "../../Camomile")

    let localedir = dir "locales"
    let unimapdir = dir "mappings"
    let charmapdir = dir "charmaps"
    let datadir = dir "database"
  end)
