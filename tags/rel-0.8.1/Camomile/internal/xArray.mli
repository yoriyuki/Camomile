(* $Id: xArray.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** XArray will be replaced by Dynarray in future. *)
  type 'a xarray
  type 'a t = 'a xarray

(* init ~bufsize len default f :
 * returned xarray has length [len], its nth-element is [f n],
 * its default value is [default].  The size of the internal buffer
 * is initially ~bufsize.  However, accessible elements are only up to [len].
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
