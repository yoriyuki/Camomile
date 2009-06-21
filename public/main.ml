

(** All-in-one, configure once at beginning module*)

module type Type = sig

  module ISet : sig
(* $Id: iSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
type t = (int * int) AvlTree.tree
type elt = int
val empty : t
val is_empty : t -> bool
val mem : int -> t -> bool

val add : int -> t -> t

val add_range : int -> int -> t -> t

val singleton : int -> t

val remove : int -> t -> t

val remove_range : int -> int -> t -> t

val union : t -> t -> t

val inter : t -> t -> t

val diff : t -> t -> t

val compl : t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val subset : t -> t -> bool

val from : int -> t -> t

val after : int -> t -> t

val until : int -> t -> t

val before : int -> t -> t

val iter : (int -> unit) -> t -> unit

val iter_range : (int -> int -> unit) -> t -> unit

val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val fold_range : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool

val exists : (int -> bool) -> t -> bool

val filter : (int -> bool) -> t -> t

val partition : (int -> bool) -> t -> t * t

val cardinal : t -> int

val elements : t -> int list

val ranges : t -> (int * int) list

val min_elt : t -> int

val max_elt : t -> int

val choose : t -> int
    end
  module IMap : sig
(* $Id: iMap.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
type +'a t = (int * int * 'a) AvlTree.tree
type key = int
val empty : 'a t
val is_empty : 'a t -> bool
val add : ?eq:('a -> 'a -> bool) -> int -> 'a -> 'a t -> 'a t
val add_range : ?eq:('a -> 'a -> bool) -> int -> int -> 'a -> 'a t -> 'a t
val find : int -> 'a t -> 'a
val remove : int -> 'a t -> 'a t
val remove_range : int -> int -> 'a t -> 'a t
val from : int -> 'a t -> 'a t
val after : int -> 'a t -> 'a t
val until : int -> 'a t -> 'a t
val before : int -> 'a t -> 'a t
val mem : int -> 'a t -> bool
val iter : (int -> 'a -> unit) -> 'a t -> unit
val iter_range : (int -> int -> 'a -> unit) -> 'a t -> unit
val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
val mapi : ?eq:('b -> 'b -> bool) -> (int -> 'a -> 'b) -> 'a t -> 'b t
val fold : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
val fold_range : (int -> int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
val set_to_map : ISet.t -> 'a -> 'a t
val domain : 'a t -> ISet.t
val map_to_set : ('a -> bool) -> 'a t -> ISet.t
    end
  module XArray : sig
(* $Id: xArray.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** XArray will be replaced by Dynarray in future. *)
  type 'a xarray
  type 'a t = 'a xarray
(* init ~bufsize len default f :
 * returned xarray has length [len], its nth-element is [f n],
 * its default value is [default]. The size of the internal buffer
 * is initially ~bufsize. However, accessible elements are only up to [len].
 * [f] is called with integers [0 ... len - 1], only once for each integer.
 * The call is in the increasing order f 0, f1, f2, ... *)
  val init : ?bufsize:int -> int -> 'a -> (int -> 'a) -> 'a xarray
(* make ~bufsize len default :
 * returns xarray filled with [default], whose default value is [default],
 * size of the internal buffer is [bufsize]. *)
  val make : ?bufsize:int -> int -> 'a -> 'a xarray
  val length : 'a xarray -> int
  val get : 'a xarray -> int -> 'a
(* set x i e :
 * set the [i]-th element of [x] to [e].
 * The length of [x] is automatically extended to [i], and
 * intermediate elements are set to the default value of [x] *)
  val set : 'a xarray -> int -> 'a -> unit
  type index
  val nth : 'a xarray -> int -> index
  val first : 'a xarray -> index
  val last : 'a xarray -> index
  val look : 'a xarray -> index -> 'a
(* next x i, prev x i :
 * operation is valid if [i] points the valid element, i.e.
 * returned value may point the location beyond valid elements by one.
 * If [i] does not point a valid element, the results are unspecified. *)
  val next : 'a t -> index -> index
  val prev : 'a t -> index -> index
  val move : 'a t -> index -> int -> index
(* test whether the given index points the valid element. *)
  val out_of_range : 'a xarray -> index -> bool
  val compare_index : 'a xarray -> index -> index -> int
(* semantics of these functions are similar to equivalents of
 * Array or Buffer. *)
  val clear : 'a xarray -> unit
  val reset : 'a xarray -> unit
  val copy : 'a xarray -> 'a xarray
  val sub : 'a xarray -> int -> int -> 'a xarray
  val add_element : 'a xarray -> 'a -> unit
  val add_array : 'a xarray -> 'a array -> unit
  val add_xarray : 'a xarray -> 'a xarray -> unit
  val append : 'a xarray -> 'a xarray -> 'a xarray
  val iter : ('a -> unit) -> 'a xarray -> unit
  val array_of : 'a xarray -> 'a array
(* shrink x len : reduce the length of [x] to [len].
 * If there is an element beyond [len], such elements are discarded. *)
  val shrink : 'a xarray -> int -> unit
    end
  module OOChannel : sig
(* $Id: oOChannel.mli,v 1.7 2004/11/03 11:21:53 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Generic input channel
  Have the same interface of Polymorphic input channel of
  http:
  All channels of Camomile having this interface must confirm
  the behaviour defined in the recommendation above.
*)
class type ['a] obj_input_channel = 
  object
    method close_in : unit -> unit
    method get : unit -> 'a 
  end
(** Generic output channel
  Have the same interface of Polymorphic output channel of
  http:
  All channels of Camomile having this interface must confirm
  the behaviour defined in the recommendation above.
*)
class type ['a] obj_output_channel = 
  object
    (** If close_oout cannot output all buffered objects, flush raises
      Failure *)
    method close_out : unit -> unit
    (** If flush cannot output all buffered objects, flush raises
      Failure *)
    method flush : unit -> unit
    method put : 'a -> unit
  end
(** Convert stream to obj_input_channel *)
class ['a] channel_of_stream : 'a Stream.t -> ['a] obj_input_channel
(** Convert obj_input_channel to stream *)
val stream_of_channel : 'a #obj_input_channel -> 'a Stream.t
(** Character(byte) input channel. Have the same interface of octet
  input channel of http:
  All channels of Camomile having this interface must confirm the
  behaviour defined in the recommendation above. In addition, all
  channels are assumed to be blocking. If you supply a non-blocking
  channel to Camomile API, the outcome is undefined.
*)
class type char_input_channel =
  object
    method input : string -> int -> int -> int
    method close_in : unit -> unit
  end
(** Character(byte) output channel. Have the same interface of octet
  input channel of http:
  All channels of Camomile having this interface must confirm the
  behaviour defined in the recommendation above. In addition, all
  channels are assumed to be blocking. If you supply a non-blocking
  channel to Camomile API, the outcome is undefined.
*)
class type char_output_channel =
  object
    method output : string -> int -> int -> int
    method flush : unit -> unit
    method close_out : unit -> unit
  end
(** Convert a polymorphic input channel to a character input channel *)
class char_input_channel_of : char #obj_input_channel ->
  char_input_channel
(** Convert a character input channel to a polymorphic input channel*)
class char_obj_input_channel_of : char_input_channel ->
  [char] obj_input_channel
(** Convert a polymorphic output channel to a character output channel *)
class char_output_channel_of : char #obj_output_channel -> char_output_channel
(** Convert a character output channel to a polymorphic output channel *)
class char_obj_output_channel_of : char_output_channel ->
  [char] obj_output_channel
(** Convert an OCaml input channel to an OO-based character input channel *)
class of_in_channel : Pervasives.in_channel -> char_input_channel
(** Convert an OCaml output channel to an OO-based character output channel *)
class of_out_channel : Pervasives.out_channel -> char_output_channel
    end
  module UChar : sig
(* $Id: uChar.mli,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Unicode (ISO-UCS) characters.
   This module implements Unicode (actually ISO-UCS) characters. All
   31-bit code points are allowed.
*)
(** Unicode characters. All 31bit code points are allowed.*)
type t
exception Out_of_range
(** [char_of u] returns the Latin-1 representation of [u].
   If [u] can not be represented by Latin-1, raises Out_of_range *)
val char_of : t -> char
(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
val of_char : char -> t
(** [code u] returns the Unicode code number of [u].
   If the value can not be represented by a positive integer,
   raise Out_of_range *)
val code : t -> int
(** [code n] returns the Unicode character with the code number [n].
   If n >= 2^32 or n < 0, raises [invalid_arg] *)
val chr : int -> t
(** [uint_code u] returns the Unicode code number of [u].
   The returned int is unsigned, that is, on 32-bits platforms,
   the sign bit is used for storing the 31-th bit of the code number. *)
external uint_code : t -> int = "%identity"
(** [chr_of_uint n] returns the Unicode character of the code number [n].
   [n] is interpreted as unsigned, that is, on 32-bits platforms,
   the sign bit is treated as the 31-th bit of the code number.
   If n exceed 31-bits values, then raise [invalid_arg]. *)
val chr_of_uint : int -> t
(** Equality by code point comparison *)
val eq : t -> t -> bool
(** [compare u1 u2] returns,
   a value > 0 if [u1] has a larger Unicode code number than [u2],
   0 if [u1] and [u2] are the same Unicode character,
   a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
val compare : t -> t -> int
(** Aliases of [type t] *)
type uchar = t
(** Alias of [uint_code] *)
val int_of : uchar -> int
(** Alias of [chr_of_uint] *)
val of_int : int -> uchar
    end
  module USet : sig
(* $Id: uSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Sets of Unicode characters, implemented as sets of intervals.
   The signature is mostly same to Set.S in stdlib *)
type t
val empty : t
val is_empty : t -> bool
val mem : UChar.t -> t -> bool
val add : UChar.t -> t -> t
(** [add_range u1 u2 s] adds the characters in the range [u1] - [u2]
   to [s]. The range is determined by the code point order. *)
val add_range : UChar.t -> UChar.t -> t -> t
val singleton : UChar.t -> t
val remove : UChar.t -> t -> t
(** [remove_range u1 u2 s] removes the characters in the range [u1] - [u2]
   from [s]. The range is determined by the code point order. *)
val remove_range : UChar.t -> UChar.t -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
(** [compl s] returns the compliment of [s]. *)
val compl : t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool
(** [from u s] returns the set of elements of [s]
   whose code points are equal or greater than [u]. *)
val from : UChar.t -> t -> t
(** [after u s] returns the set of elements of [s]
   whose code points are greater than [u]. *)
val after : UChar.t -> t -> t
(** [until u s] returns the set of elements of [s]
   whose code points are equal or smaller than [u]. *)
val until : UChar.t -> t -> t
(** [until u s] returns the set of elements of [s]
   whose code points are smaller than [u]. *)
val before : UChar.t -> t -> t
val iter : (UChar.t -> unit) -> t -> unit
(** [iter_range proc s] feeds the intervals contained in [s] to
   [proc] in increasing order. The intervals given to [proc]
   are always separated by the character not in [s]. *)
val iter_range : (UChar.t -> UChar.t -> unit) -> t -> unit
val fold : (UChar.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_range f s x] is equivalent to
   [f u_i u_(i+1) (... (f u_3 u_4 (f u_1 u_2 x)))] if [s] is consisted of
   the intervals [u1]-[u2], [u3]-[u4], ..., [u_i]-[u_(i + 1)]
   in increasing order. The intervals given to [proc]
   are always separated by the character not in [s]. *)
val fold_range : (UChar.t -> UChar.t -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (UChar.t -> bool) -> t -> bool
val exists : (UChar.t -> bool) -> t -> bool
val filter : (UChar.t -> bool) -> t -> t
val partition : (UChar.t -> bool) -> t -> t * t
val cardinal : t -> int
val elements : t -> UChar.t list
(** The list of the intervals contained in the set.
   The returned intervals are always separated
   by the character not in [s]. *)
val ranges : t -> (UChar.t * UChar.t) list
val min_elt : t -> UChar.t
val max_elt : t -> UChar.t
(** Returns a element roughly in the middle of the set.
   It is not guaranteed to return the same element for
   the sets with the same elements *)
val choose : t -> UChar.t
val uset_of_iset : ISet.t -> t
val iset_of_uset : t -> ISet.t
    end
  module UMap : sig
(* $Id: uMap.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Maps over Unicode characters. *)
type 'a t
val empty : 'a t
val is_empty : 'a t -> bool
(** [add ?eq u v m] returns the new map which is same to [m]
   except it maps [u] to some value [v'] which satisfies [eq v v'].
   If [eq] is not supplied, structural equality is used. *)
val add : ?eq:('a -> 'a -> bool) -> UChar.t -> 'a -> 'a t -> 'a t
(** [add ?eq u1 u2 v m] returns the new map which is same to [m]
   except it maps characters in the range [u1]-[u2]
   to some value [v'] which satisfies [eq v v'].
   If [eq] is not supplied, structural equality is used. *)
val add_range : ?eq:('a -> 'a -> bool) ->
  UChar.t -> UChar.t -> 'a -> 'a t -> 'a t
val find : UChar.t -> 'a t -> 'a
val remove : UChar.t -> 'a t -> 'a t
(** [remove_range u1 u2 m] removes [u1]-[u2] from the domain of [m] *)
val remove_range : UChar.t -> UChar.t -> 'a t -> 'a t
(** [from u m] restricts the domain of [m] to the characters whose
   code points are equal or greater than [u]. *)
val from : UChar.t -> 'a t -> 'a t
(** [after u m] restricts the domain of [m] to the characters whose
   code points are greater than [u]. *)
val after : UChar.t -> 'a t -> 'a t
(** [until u m] restricts the domain of [m] to the characters whose
   code points are equal or smaller than [u]. *)
val until : UChar.t -> 'a t -> 'a t
(** [before u m] restricts the domain of [m] to the characters whose
   code points are smaller than [u]. *)
val before : UChar.t -> 'a t -> 'a t
val mem : UChar.t -> 'a t -> bool
val iter : (UChar.t -> 'a -> unit) -> 'a t -> unit
(** [iter proc m] : For each contingent region [u1]-[u2]
   that is mapped to a constant [v], [proc u1 u2 v] is called.
   The order of call is determined by increasing order on [u1]. *)
val iter_range : (UChar.t -> UChar.t -> 'a -> unit) -> 'a t -> unit
(** [map ?eq f m] and [mapi ?eq f m] : Similar to [map] and [mapi]
   in stdlib Map, but if the map [m'] is returned,  it is only guaranteed 
   that [eq (find u m') (f (find u m ))] is true for [map] and 
   [eq (find u m') (f u (find u m ))] is true for [mapi].  If [eq] is
   not specified, structural equality is used. *)
val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
val mapi : ?eq:('b -> 'b -> bool) -> (UChar.t -> 'a -> 'b) -> 'a t -> 'b t
val fold : (UChar.t -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [fold_range f m x] is equivalent to
   [f u_(2n) u_(2n+1) v_n (... (f u_1 u_2 v_1 x))] where all characters in
   the range [u_(2k)]-[u_(2k+1)] are mapped to [v_k] and
   [u_1] < [u_3] < ... in code point order.
   For each range [u_(2k)]-[u_(2k+1)] is separated by a character
   which is not mapped to [v_k]. *)
val fold_range : (UChar.t -> UChar.t -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** Constant map.*)
val set_to_map : USet.t -> 'a -> 'a t
(** Domain. *)
val domain : 'a t -> USet.t
(** [map_to_set p m] returns the set of characters which are mapped
   to values satisfying the predicate [p] by [m]. *)
val map_to_set : ('a -> bool) -> 'a t -> USet.t
val umap_of_imap : 'a IMap.t -> 'a t
val imap_of_umap : 'a t -> 'a IMap.t
    end
  module UCharTbl : sig
(* $Id: uCharTbl.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Fast lookup tables. Accessible by constant time. *)
  type 'a tbl
  type 'a t = 'a tbl
  val get : 'a tbl -> UChar.t -> 'a
  module type Type = sig
    type elt
    type t = elt tbl
    val get : elt tbl -> UChar.t -> elt
(** [of_map def m] creates the table which has the same value to [m].
   The table returns [def] for the characters for which [m] is undefined. *)
    val of_map : elt -> elt UMap.t -> t
  end
(** Equality and hash are necessary for table generation. *)
  module Make :
  functor (H : Hashtbl.HashedType) -> (Type with type elt = H.t)
(** Tables for boolean values. *)
  module Bool : sig
    type t
    val get : t -> UChar.t -> bool
    val of_set : USet.t -> t
  end
(** Tables for small (< 256, >=0) integers *)
  module Bits : sig
    type t
    val of_map : int -> int UMap.t -> t
    val get : t -> UChar.t -> int
  end
(** Tables for integers. If integers are not span the whole 31-bit or
63-bit values, [Bytes.t] is more space efficient than [int tbl]. *)
  module Bytes : sig
    type t
    val of_map : int -> int UMap.t -> t
    val get : t -> UChar.t -> int
  end
(** Tables for bytes. *)
  module Char : sig
    type t
    val of_map : char -> char UMap.t -> t
    val get : t -> UChar.t -> char
  end
    end
  module UnicodeString : sig
(* $Id: unicodeString.mli,v 1.3 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Signature for Unicode strings.
   {!UText}, {!XString}, {!UTF8}, {!UTF16}, {!UCS4}
   have matched signatures to UStorage
   and satisfy the semantics described below. If users want to supply
   their own Unicode strings, please design the module with the
   following signature and properties. *)
module type Type = sig
(** The type of string. *)
  type t
(** [get t i] : [i]-th character of the storage.*)
  val get : t -> int -> UChar.t
(** [init len f] creates a new storage.
   the returned storage has length [len], its nth-element is [f n].
   [f] is called with integers [0 ... len - 1], only once for each integer.
   The call is in the increasing order f 0, f 1, f 2, ... *)
  val init : int -> (int -> UChar.t) -> t
(** The number of Unicode characters in the storage *)
  val length : t -> int
(** locations in storages.*)
  type index
(** [look t i] : The character in the location [i] of [t].*)
  val look : t -> index -> UChar.t
(** [nth t n] : the location of the [n]-th character in [t].*)
  val nth : t -> int -> index
(** [next x i, prev x i] :
   The operation is valid if [i] points the valid element, i.e. the
   returned value may point the location beyond valid elements by one.
   If [i] does not point a valid element, the results are unspecified. *)
  val next : t -> index -> index
  val prev : t -> index -> index
(* [out_of_range t i] tests whether [i] is inside of [t]. *)
  val out_of_range : t -> index -> bool
  val iter : (UChar.t -> unit) -> t -> unit
(* Code point comparison *)
  val compare : t -> t -> int
(** The location of the first character in the storage. *)
  val first : t -> index
(** The location of the last character in the storage. *)
  val last : t -> index
(** [move t i n] :
   if [n] >= 0, then returns [n]-th character after [i] and
   otherwise returns -[n]-th character before [i].
   If there is no such character, or [i] does not point
   a valid character, the result is unspecified. *)
  val move : t -> index -> int -> index
(** [compare_index t i j] returns
   a positive integer if [i] is the location placed after [j] in [t],
   0 if [i] and [j] point the same location, and
   a negative integer if [i] is the location placed before [j] in [t]. *)
  val compare_index : t -> index -> index -> int
(** Character buffers. Similar to Buffer. *)
  module Buf : sig
    type buf
(** [create n] creates the buffer. [n] is used to determine
   the initial size of the buffer. The meaning of [n] differs from
   modules to modules. *)
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
end
    end
  module UText : sig
(* $Id: uText.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** An implementation of Unicode string.
   Internally, it uses integer array.
   The semantics matches the description of UStorage. *)
(** Phantom type for distinguishing mutability *)
  type mutability = [ `Mutable | `Immutable ]
  type 'a text
  type utext = [`Immutable] text
  type ustring = [`Mutable] text
  type t = utext
  val utext_of_ustring : ustring -> utext
  val ustring_of_utext : utext -> ustring
  val get : 'a text -> int -> UChar.t
(** [set s i u] sets the [i]-th character in [s] to [u]. *)
  val set : ustring -> int -> UChar.t -> unit
  type index
  val look : 'a text -> index -> UChar.t
  val nth : 'a text -> int -> index
  val first : 'a text -> index
  val last : 'a text -> index
  val out_of_range : 'a text -> index -> bool
  val compare_index : 'a text -> index -> index -> int
  val next : 'a text -> index -> index
  val prev : 'a text -> index -> index
  val move : 'a text -> index -> int -> index
  val length : 'a text -> int
(** Conversion from Latin-1 strings. *)
  val of_string : string -> utext
  val init : int -> (int -> UChar.t) -> utext
  val init_ustring : int -> (int -> UChar.t) -> ustring
(** The semantics of these function are similar to
   the equivalents of string. *)
  val make : int -> UChar.t -> ustring
  val copy : ustring -> ustring
  val sub : 'a text -> int -> int -> 'a text
  val fill : ustring -> int -> int -> UChar.t -> unit
  val blit : 'a text -> int -> ustring -> int -> int -> unit
  val append : 'a text -> 'b text -> 'a text
  val iter : (UChar.t -> unit) -> 'a text -> unit
  val compare : 'a text -> 'b text -> int
  module Buf : sig
    type buf
(** [create n] creates the buffer which initially can contain
   [n] Unicode characters. *)
    val create : int -> buf
    val contents : buf -> t
    val contents_string : buf -> ustring
    val length : buf -> int
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> 'a text -> unit
    val add_buffer : buf -> buf -> unit
  end
    end
  module XString : sig
(* $Id: xString.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** eXtensible Unicode string.
   The semantics matches the description of UStorage.
   The detail may be going to change.*)
  type xstring
  type t = xstring
  val get : xstring -> int -> UChar.t
  val set : xstring -> int -> UChar.t -> unit
  val length : xstring -> int
  val init : int -> (int -> UChar.t) -> xstring
  type index
  val look : xstring -> index -> UChar.t
  val nth : xstring -> int -> index
  val first : xstring -> index
  val last : xstring -> index
  val out_of_range : xstring -> index -> bool
  val next : xstring -> index -> index
  val prev : xstring -> index -> index
  val move : xstring -> index -> int -> index
  val compare_index : xstring -> index -> index -> int
  val make : ?bufsize:int -> int -> UChar.t -> xstring
  val clear : xstring -> unit
  val reset : xstring -> unit
  val copy : xstring -> xstring
  val sub : xstring -> int -> int -> xstring
  val add_char : xstring -> UChar.t -> unit
  val add_text : xstring -> 'a UText.text -> unit
  val add_xstring : xstring -> xstring -> unit
  val shrink : xstring -> int -> unit
  val append : xstring -> xstring -> xstring
  val utext_of : xstring -> UText.t
  val ustring_of : xstring -> UText.ustring
  val iter : (UChar.t -> unit) -> xstring -> unit
  val compare : t -> t -> int
  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val length : buf -> int
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
    end
  module SubText : sig
(* $Id: subText.mli,v 1.2 2004/09/04 16:08:40 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Sub-texts, parts of original (ur-) texts.
   The signature and semantics matches those of UStorage. *)
module type Type = sig
  type t
  val get : t -> int -> UChar.t
  val init : int -> (int -> UChar.t) -> t
  val length : t -> int
  type index
  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val first : t -> index
  val last : t -> index
  val next : t -> index -> index
  val prev : t -> index -> index
  val move : t -> index -> int -> index
  val out_of_range : t -> index -> bool
  val compare_index : t -> index -> index -> int
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int
  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
(** The type of original texts. *)
  type ur_text
(** The type of indexes of original texts. *)
  type ur_index
(** [refer t i j] returns the part of [t] from [i] until [j].
   The character pointed by [j] is not included in the result.
   If [j] is equal to [i] or located before [j], the result is
   an empty string. *)
  val refer : ur_text -> ur_index -> ur_index -> t
(** [excerpt t] copies the contents of [t] as a new ur_text. *)
  val excerpt : t -> ur_text
(** [context t] returns the tuple [(s, i, j)] such that
   [t = refer s i j]. *)
  val context : t -> ur_text * ur_index * ur_index
(** Conversion from indexes of sub-texts to ur_texts. *)
  val ur_index_of : t -> index -> ur_index
end
module Make : functor (Text : UnicodeString.Type) ->
  (Type with type ur_text = Text.t and type ur_index = Text.index)
    end
  module ULine : sig
(* $Id: uLine.mli,v 1.4 2004/04/14 18:11:07 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Line I/O, conversion of line separators. *)
open OOChannel
(** Line separators.
   - [`CR] specifies carriage return.
   - [`LF] specifies linefeed.
   - [`CRLF] specifies the sequence of carriage return and linefeed.
   - [`NEL] specifies next line (\u0085).
   - [`LS] specifies Unicode line separator (\u2028).
   - [`PS] specifies Unicode paragraph separator (\u2029). *)
type separator =
  [ `CR
  | `LF
  | `CRLF
  | `NEL
  | `LS
  | `PS ]
(** [new input separator input_obj] creates the new input channel object
   {!OOChannel.obj_input_channel} which reads from [input_obj] and
   converts line separators (all of CR, LF, CRLF, NEL, LS, PS) to
   [separator]. *)
class input : separator ->
  UChar.t #obj_input_channel -> [UChar.t] obj_input_channel
(** [new output separator output_obj] creates the new output channel
   object {!OOChannel.obj_output_channel} which receives Unicode characters
   and converts line separators (all of CR, LF, CRLF, NEL, LS, PS) to
   [separator]. *)
class output : separator ->
  UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
module type Type = sig
  type text
(** [new input_line input_obj] creates the new input channel object
   {!OOChannel.obj_input_channel} which reads Unicode characters
   from [input_obj] and output lines. All of CR, LF, CRLF, NEL, LS, PS,
   as well as FF (formfeed) are recognised as a line separator. *)
  class input_line : UChar.t #obj_input_channel -> [text] obj_input_channel
(** [new output_line ~sp output_obj] create the new output channel object
   {!OOChannel.obj_output_channel} which output each line to [output_obj]
   using [sp] as a line separator.
   If [sp] is omitted, linefeed (LF) is used. *)
  class output_line : ?sp:separator ->
      UChar.t #obj_output_channel -> [text] obj_output_channel
end
module Make : functor (Text : UnicodeString.Type) ->
  (Type with type text = Text.t)
    end
  module Locale : sig
(* $Id: locale.mli,v 1.13 2004/08/29 04:48:21 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)
(** Camomile has a locale system similar to Java.
   A locale is a string with a form as
   "<LANG>_<COUNTRY>_<MODIFIER>..." where <LANG> is
   a 2-letter ISO 639 language code, <COUNTRY> is a 2-letter ISO 3166
   country code. Some field may not present. *)
(** Type of locales. *)
type t = string
(** [read root suffix reader locale]
   reads locale information using [reader].
   Locale data is supposed to reside in [root] directory with
   the name [locale].[suffix].
   [reader] takes [in_channel] as an argument and read data from in_channel.
   If data is not found, then [reader] should raise Not_found.
   If the file is not found or [reader] raises Not_found, then
   more generic locales are tried.
   For example, if fr_CA.[suffix] is not found, then [read] tries fr.[suffix].
   If fr.[suffix] is also not found, then the file [root].[suffix] is tried.
   Still the data is not found, then [Not_found] is raised. *)
val read : string -> string -> (in_channel -> 'a) -> string -> 'a
(** [contain loc1 loc2] :
   If [loc1] is contained in [loc2] then true otherwise false.
   For example, "fr" is contained in "fr_CA" while "en_CA"
   does not contain "fr" *)
val contain : string -> string -> bool
    end
  module CharEncoding : CharEncoding.Interface
  module UTF8 : sig
(*
 * UTF-8 - UTF-8 encoded Unicode string
 * Copyright 2002, 2003 (C) Yamagata Yoriyuki.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *)
(** UTF-8 encoded Unicode strings.
   The Module for UTF-8 encoded Unicode strings.
*)
(** UTF-8 encoded Unicode strings. The type is normal string. *)
type t = string
exception Malformed_code
(** [validate s]
   successes if s is valid UTF-8, otherwise raises Malformed_code.
   Other functions assume strings are valid UTF-8, so it is prudent
   to test their validity for strings from untrusted origins. *)
val validate : t -> unit
(* All functions below assume string are valid UTF-8. If not,
 * the result is unspecified. *)
(** [get s n] returns [n]-th Unicode character of [s].
   The call requires O(n)-time. *)
val get : t -> int -> UChar.t
(** [init len f]
   returns a new string which contains [len] Unicode characters.
   The i-th Unicode character is initialized by [f i] *)
val init : int -> (int -> UChar.t) -> t
(** [length s] returns the number of Unicode characters contained in s *)
val length : t -> int
(** Positions in the string represented by the number of bytes from the head.
   The location of the first character is [0] *)
type index = int
(** [nth s n] returns the position of the [n]-th Unicode character.
   The call requires O(n)-time *)
val nth : t -> int -> index
(** The position of the head of the first Unicode character. *)
val first : t -> index
(** The position of the head of the last Unicode character. *)
val last : t -> index
(** [look s i]
   returns the Unicode character of the location [i] in the string [s]. *)
val look : t -> index -> UChar.t
(** [out_of_range s i]
   tests whether [i] is a position inside of [s]. *)
val out_of_range : t -> index -> bool
(** [compare_index s i1 i2] returns
   a value < 0 if [i1] is the position located before [i2],
   0 if [i1] and [i2] points the same location,
   a value > 0 if [i1] is the position located after [i2]. *)
val compare_index : t -> index -> index -> int
(** [next s i]
   returns the position of the head of the Unicode character
   located immediately after [i].
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character after [i],
   the position outside [s] is returned.
   If [i] is not inside of [s], the behaviour is unspecified. *)
val next : t -> index -> index
(** [prev s i]
   returns the position of the head of the Unicode character
   located immediately before [i].
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character before [i],
   the position outside [s] is returned.
   If [i] is not inside of [s], the behaviour is unspecified. *)
val prev : t -> index -> index
(** [move s i n]
   returns [n]-th Unicode character after [i] if n >= 0,
   [n]-th Unicode character before [i] if n < 0.
   If there is no such character, the result is unspecified. *)
val move : t -> index -> int -> index
(** [iter f s]
   applies [f] to all Unicode characters in [s].
   The order of application is same to the order
   of the Unicode characters in [s]. *)
val iter : (UChar.t -> unit) -> t -> unit
(** Code point comparison by the lexicographic order.
   [compare s1 s2] returns
   a positive integer if [s1] > [s2],
   0 if [s1] = [s2],
   a negative integer if [s1] < [s2]. *)
val compare : t -> t -> int
(** Buffer module for UTF-8 strings *)
module Buf : sig
  (** Buffers for UTF-8 strings. *)
  type buf
  (** [create n] creates the buffer with the initial size [n]-bytes. *)
  val create : int -> buf
  (* The rest of functions is similar to the ones of Buffer in stdlib. *)
  (** [contents buf] returns the contents of the buffer. *)
  val contents : buf -> t
  (** Empty the buffer,
     but retains the internal storage which was holding the contents *)
  val clear : buf -> unit
  (** Empty the buffer and de-allocate the internal storage. *)
  val reset : buf -> unit
  (** Add one Unicode character to the buffer. *)
  val add_char : buf -> UChar.t -> unit
  (** Add the UTF-8 string to the buffer. *)
  val add_string : buf -> t -> unit
  (** [add_buffer b1 b2] adds the contents of [b2] to [b1].
     The contents of [b2] is not changed. *)
  val add_buffer : buf -> buf -> unit
end with type buf = Buffer.t
    end
  module UTF16 : sig
(* $Id: uTF16.mli,v 1.6 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** UTF-16 encoded string. the type is the bigarray of 16-bit integers.
   The characters must be 21-bits code points, and not surrogate points,
   0xfffe, 0xffff.
   Bigarray.cma or Bigarray.cmxa must be linked when this module is used. *)
type t =
    (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
exception Malformed_code
(** [validate s]
   If [s] is valid UTF-16 then successes otherwise raises [Malformed_code].
   Other functions assume strings are valid UTF-16, so it is prudent
   to test their validity for strings from untrusted origins. *)
val validate : t -> unit
(** All functions below assume strings are valid UTF-16. If not,
   the result is unspecified. *)
(** [get s n] returns [n]-th Unicode character of [s].
   The call requires O(n)-time. *)
val get : t -> int -> UChar.t
exception Out_of_range
(** [init len f]
   returns a new string which contains [len] Unicode characters.
   The i-th Unicode character is initialized by [f i]
   if the character is not representable, raise [Out_of_range]. *)
val init : int -> (int -> UChar.t) -> t
(** [length s] returns the number of Unicode characters contained in s *)
val length : t -> int
(** Positions in the string represented by the number of 16-bit unit
   from the head.
   The location of the first character is [0] *)
type index = int
(** [nth s n] returns the position of the [n]-th Unicode character.
   The call requires O(n)-time *)
val nth : t -> int -> index
(** [first s] : The position of the head of the last Unicode character. *)
val first : t -> index
(** [last s] : The position of the head of the last Unicode character. *)
val last : t -> index
(** [look s i ]
   returns the Unicode character of the location [i] in the string [s]. *)
val look : t -> index -> UChar.t
(** [out_of_range s i] tests whether [i] is inside of [s]. *)
val out_of_range : t -> index -> bool
(** [compare_aux s i1 i2] returns
 - If [i1] is the position located before [i2], a value < 0,
 - If [i1] and [i2] points the same location, 0,
 - If [i1] is the position located after [i2], a value > 0.
*)
val compare_index : t -> index -> index -> int
(** [next s i]
   returns the position of the head of the Unicode character
   located immediately after [i].
 - If [i] is a valid position, the function always success.
 - If [i] is a valid position and there is no Unicode character after [i],
   the position outside [s] is returned.
 - If [i] is not a valid position, the behaviour is undefined.
*)
val next : t -> index -> index
(** [prev s i]
   returns the position of the head of the Unicode character
   located immediately before [i].
   - If [i] is a valid position, the function always success.
   - If [i] is a valid position and there is no Unicode character before [i],
   the position outside [s] is returned.
   - If [i] is not a valid position, the behaviour is undefined.
*)
val prev : t -> index -> index
(* [move s i n]
 - If n >= 0, returns [n]-th Unicode character after [i].
 - If n < 0, returns [-n]-th Unicode character before [i].
 0 If there is no such character, the result is unspecified.
*)
val move : t -> index -> int -> index
(** [iter f s]
   Apply [f] to all Unicode characters in [s].
   The order of application is same to the order
   in the Unicode characters in [s]. *)
val iter : (UChar.t -> unit) -> t -> unit
(** Code point comparison *)
val compare : t -> t -> int
(** Buffer module for UTF-16 *)
module Buf : sig
  type buf
  (** create n : creates the buffer with the initial size [n]. *)
  val create : int -> buf
  (** The rest of functions is similar to the ones of Buffer in stdlib. *)
  val contents : buf -> t
  val clear : buf -> unit
  val reset : buf -> unit
  (** if the character is not representable, raise Out_of_range *)
  val add_char : buf -> UChar.t -> unit
  val add_string : buf -> t -> unit
  val add_buffer : buf -> buf -> unit
end
    end
  module UCS4 : sig
(* $Id: uCS4.mli,v 1.5 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** UCS4 encoded string. The type is the bigarray of 32-bit integers.
   Bigarray.cma or Bigarray.cmxa must be linked when this module is used. *)
type t =
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
exception Malformed_code
(** [validate s]
   If [s] is valid UCS4 then successes otherwise raises [Malformed_code].
   Other functions assume strings are valid UCS4, so it is prudent
   to test their validity for strings from untrusted origins. *)
val validate : t -> unit
(** All functions below assume strings are valid UCS4. If not,
   the result is unspecified. *)
(** [get s n] returns [n]-th Unicode character of [s]. *)
val get : t -> int -> UChar.t
(** [init len f]
   returns a new string which contains [len] Unicode characters.
   The i-th Unicode character is initialised by [f i] *)
val init : int -> (int -> UChar.t) -> t
(** [length s] returns the number of Unicode characters contained in [s] *)
val length : t -> int
(** Positions in the string represented by the number of characters
   from the head.
   The location of the first character is [0] *)
type index = int
(** [nth s n] returns the position of the [n]-th Unicode character.
   The call requires O(n)-time *)
val nth : t -> int -> index
(** [first s] : The position of the head of the last Unicode character. *)
val first : t -> index
(** [last s] : The position of the head of the last Unicode character. *)
val last : t -> index
(** [look s i]
   returns the Unicode character of the location [i] in the string [s]. *)
val look : t -> index -> UChar.t
(** [out_of_range s i]
   tests whether [i] points the valid position of [s]. *)
val out_of_range : t -> index -> bool
(** [compare_aux s i1 i2] returns
   If [i1] is the position located before [i2], a value < 0,
   If [i1] and [i2] points the same location, 0,
   If [i1] is the position located after [i2], a value > 0. *)
val compare_index : t -> index -> index -> int
(** [next s i]
   returns the position of the head of the Unicode character
   located immediately after [i].
   If [i] is a valid position, the function always success.
   If [i] is a valid position and there is no Unicode character after [i],
   the position outside [s] is returned.
   If [i] is not a valid position, the behaviour is undefined. *)
val next : t -> index -> index
(** [prev s i]
   returns the position of the head of the Unicode character
   located immediately before [i].
   If [i] is a valid position, the function always success.
   If [i] is a valid position and there is no Unicode character before [i],
   the position outside [s] is returned.
   If [i] is not a valid position, the behaviour is undefined. *)
val prev : t -> index -> index
(** [move s i n] :
   If n >= 0, returns [n]-th Unicode character after [i].
   If n < 0, returns [-n]-th Unicode character before [i].
   If there is no such character, the result is unspecified. *)
val move : t -> index -> int -> index
(** [iter f s] :
   Apply [f] to all Unicode characters in [s].
   The order of application is same to the order
   in the Unicode characters in [s]. *)
val iter : (UChar.t -> unit) -> t -> unit
(** Code point comparison *)
val compare : t -> t -> int
(** Buffer module for UCS4 *)
module Buf : sig
  type buf
  (** [create n] creates the buffer with the initial size [n]. *)
  val create : int -> buf
  (** The rest of functions is similar to the ones of Buffer in stdlib. *)
  val contents : buf -> t
  val clear : buf -> unit
  val reset : buf -> unit
  val add_char : buf -> UChar.t -> unit
  val add_string : buf -> t -> unit
  val add_buffer : buf -> buf -> unit
end
    end
  module UPervasives : sig
(* $Id: uPervasives.mli,v 1.1 2004/09/04 16:06:25 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
type uchar = UChar.t
(** Aliases for UChar.uint_code, UChar.chr_of_uint *)
val int_of_uchar : uchar -> int
val uchar_of_int : int -> uchar
val escaped_uchar : uchar -> string
val escaped_utf8 : string -> string
val printer_utf8 : Format.formatter -> string -> unit
val printer_uchar : Format.formatter -> uchar -> unit
    end
  module URe : sig
(* $Id: uRe.mli,v 1.6 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Regular expression engine. *)
(** Abstract syntax trees of regular expressions. *)
type regexp =
  [ `Alt of regexp * regexp
  | `Seq of regexp * regexp
  | `Rep of regexp
  | `Repn of regexp * int * int option
  | `After of regexp
  | `Before of regexp
  | `Epsilon
  | `Group of regexp
  | `OneChar
  | `String of UChar.t list
  | `Set of USet.t
  | `BoS
  | `EoS ]
(** Match semantics. *)
type match_semantics = [ `First | `Shortest | `Longest ]
(** Remove [`Group] from the regular expressions. *)
val no_group : regexp -> regexp
module type Type = sig
  type text
  type index
  type compiled_regexp
  module SubText :
    SubText.Type with type ur_text = text and type ur_index = index
(** Compile regular expressions. *)
  val compile : regexp -> compiled_regexp
(** [regexp_match ?sem r t i] tries matching [r] and substrings
   of [t] beginning from [i]. If match successes, [Some g] is
   returned where [g] is the array containing the matched
   string of [n]-th group in the [n]-element.
   The matched string of the whole [r] is stored in the [0]-th element.
   If matching fails, [None] is returned. *)
  val regexp_match : ?sem:match_semantics ->
    compiled_regexp -> text -> index -> SubText.t option array option
(** [string_match r t i] tests whether [r] can match a substring
   of [t] beginning from [i]. *)
  val string_match : compiled_regexp -> text -> index -> bool
(** [search_forward ?sem r t i] searches a substring of [t]
   matching [r] from [i]. The returned value is similar to
   {!URe.Type.regexp_match}. *)
  val search_forward : ?sem:match_semantics ->
      compiled_regexp -> text -> index -> SubText.t option array option
end
module Make : functor (Text : UnicodeString.Type) ->
  Type with type text = Text.t and type index = Text.index
    end
  module UCharInfo : UCharInfo.Type
  module UNF : sig
    module type Type = UNF.Type
    module Make (Text : UnicodeString.Type) :
 Type with type text = Text.t and type index = Text.index
  end
  module UCol : sig
(** How variables are handled *)
    type variable_option =
 [ `Blanked
      | `Non_ignorable
      | `Shifted
      | `Shift_Trimmed ]
(** Strength of comparison. For European languages, each strength
    roughly means as
    `Primary : Ignore accents and case
    `Secondary : Ignore case but accents are counted in.
    `Tertiary : Accents and case are counted in.
    For the case of `Shifted, `Shift_Trimmed, there is the fourth strength.
    `Quaternary : Variables such as - (hyphen) are counted in. *)
    type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]
    module type Type = UCol.Type
    module Make (Text : UnicodeString.Type) :
 Type with type text = Text.t and type index = Text.index
  end
  module CaseMap : sig
    module type Type = CaseMap.Type
    module Make (Text : UnicodeString.Type) : (Type with type text = Text.t)
  end
  module UReStr : UReStr.Interface
end
module Make(Config : ConfigInt.Type) = struct
  module ISet = ISet
  module IMap = IMap
  module XArray = XArray
  module OOChannel = OOChannel
  module UChar = UChar
  module USet = USet
  module UMap = UMap
  module UCharTbl = UCharTbl
  module UnicodeString = UnicodeString
  module UText = UText
  module XString = XString
  module SubText = SubText
  module ULine = ULine
  module Locale = Locale
  module CharEncoding = CharEncoding.Configure(Config)
  module UTF8 = UTF8
  module UTF16 = UTF16
  module UCS4 = UCS4
  module UPervasives = UPervasives
  module URe = URe
  module UCharInfo = UCharInfo.Make(Config)
  module UNF = struct
    module type Type = UNF.Type
    module Make = UNF.Make(Config)
  end
  module UCol = struct
    type variable_option =
 [ `Blanked
      | `Non_ignorable
      | `Shifted
      | `Shift_Trimmed ]
    type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]
    module type Type = UCol.Type
    module Make = UCol.Make(Config)
  end
  module CaseMap = struct
    module type Type = CaseMap.Type
    module Make = CaseMap.Make(Config)
  end
  module UReStr = UReStr.Configure(Config)
end
