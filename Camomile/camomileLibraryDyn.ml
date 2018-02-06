(* Copyright 2010, Sylvain Le Gal, Yamagata Yoriyuki, distributed with LGPL *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

module Config = struct

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
end

module Camomile = CamomileLibrary.Make(Config)
