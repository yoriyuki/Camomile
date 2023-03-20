let get_path site = match site with [path] -> path | _ -> assert false
let datadir = get_path Sites.Sites.database
let localedir = get_path Sites.Sites.locales
let charmapdir = get_path Sites.Sites.charmaps
let unimapdir = get_path Sites.Sites.mappings
