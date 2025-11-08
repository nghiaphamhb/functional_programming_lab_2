module type HASHABLE = sig
  type t

  val hash : t -> int (* hash function *)
  val equal : t -> t -> bool (* equality function *)
end

module type SET = sig
  type elt (* eg: int *)
  type t (* eg: int string *)

  val empty : t (* create empty set *)
  (** constructors and queries *)

  val is_empty : t -> bool (* is set empty? *)
  val cardinal : t -> int (* number of elements*)
  val mem : elt -> t -> bool (* is element in set? *)

  val add : elt -> t -> t
  (** basic edits*)

  val remove : elt -> t -> t

  val iter : (elt -> unit) -> t -> unit (* iter f s *)
  (** high-order operators*)

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a (* fold_left f acc s *)
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a (* fold_right f s acc *)
  val filter : (elt -> bool) -> t -> t (* filter p s *)
  val map : (elt -> elt) -> t -> t (* map f s *)

  val union : t -> t -> t (* union s1 s2 *)
  (** Monoid over union *)

  val equal_set : t -> t -> bool (* equal_set s1 s2 *)
  val subset : t -> t -> bool (* subset s1 s2 *)

  val to_list : t -> elt list (* convert set to list *)
  (** Utilities (for tests / debug) *)

  val of_list : elt list -> t (* convert list to set *)
end

module Make (H : HASHABLE) : SET with type elt = H.t
