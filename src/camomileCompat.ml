module CamomileLibrary = struct
  [@@@ocaml.deprecated
  "this module is deprecated, please update to the most recent camomile API"]

  include Camomile

  module ConfigInt = struct
    module type Type = CamomileLib.Config.Type
  end
end

module CamomileDefaultConfig = Camomile.DefaultConfig
module CamomileLibraryDefault = CamomileLibrary
module CamomileLibraryDyn = CamomileLibraryDefault
