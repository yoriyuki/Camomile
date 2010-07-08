type set_notation =
  [ `Set of USet.t
  | `Property of string
  | `Intr of set_notation * set_notation
  | `Union of set_notation * set_notation
  | `Diff of set_notation * set_notation
  | `Compl of set_notation ]

type tree =
  [ `Alt of tree * tree
  | `Seq of tree * tree
  | `Rep of tree
  | `Repn of tree * int * int option
  | `After of tree   
  | `Before of tree
  | `Epsilon
  | `Group of tree
  | `OneChar
  | `String of UChar.t list
  | `Set of USet.t
  | `SetNotation of set_notation
  | `BoS
  | `EoS ]

