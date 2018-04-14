open CamomileLibrary.Private

module Unidata = Unidata.Make(Camomileconfig)
module Charmap = Charmap.Configure(Camomileconfig)
module Unimap = Unimap.Make(Camomileconfig)
