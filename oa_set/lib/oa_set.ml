(* oa_set.ml *)
module type HASHABLE = sig
  type t

  val hash : t -> int
  (* hash function (not strictly needed here, but kept for API compatibility) *)

  val equal : t -> t -> bool (* equality function for elements *)
end

module type SET = sig
  type elt (* element type, e.g. int *)
  type t (* set type, e.g. set of elt *)

  val empty : t (* create empty set *)

  (* basic queries *)
  val is_empty : t -> bool (* is the set empty? *)
  val cardinal : t -> int (* number of elements in the set *)
  val mem : elt -> t -> bool (* membership test *)

  (* basic updates (all are pure: return new sets) *)
  val add : elt -> t -> t
  val remove : elt -> t -> t

  (* higher-order operations *)
  val iter : (elt -> unit) -> t -> unit
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t (* type-preserving map *)

  (* monoid structure over union *)
  val union : t -> t -> t

  (* set relations *)
  val equal_set : t -> t -> bool
  val subset : t -> t -> bool

  (* conversion helpers (mainly for tests / debugging) *)
  val to_list : t -> elt list
  val of_list : elt list -> t
end

module Make (H : HASHABLE) : SET with type elt = H.t = struct
  type elt = H.t

  (* Internal representation:
     - a set is a list of unique elements
     - invariants:
       * no duplicates according to H.equal
       * order is not important from the abstract point of view       *)
  type t = elt list

  (* Empty set is represented by the empty list *)
  let empty : t = []
  let is_empty (s : t) : bool = s = []
  let cardinal (s : t) : int = List.length s

  (* Membership test using H.equal *)
  let mem (x : elt) (s : t) : bool = List.exists (fun y -> H.equal x y) s

  (* Pure add:
     - if element already present, return the same set
     - otherwise, cons it to the front *)
  let add (x : elt) (s : t) : t = if mem x s then s else x :: s

  (* Pure remove:
     - filter out all elements equal to x *)
  let remove (x : elt) (s : t) : t = List.filter (fun y -> not (H.equal x y)) s

  (* Iteration over all elements *)
  let iter (f : elt -> unit) (s : t) : unit = List.iter f s

  (* Left fold over the underlying list *)
  let fold_left (f : 'a -> elt -> 'a) (acc : 'a) (s : t) : 'a =
    List.fold_left f acc s

  (* Right fold over the underlying list *)
  let fold_right (f : elt -> 'a -> 'a) (s : t) (acc : 'a) : 'a =
    List.fold_right f s acc

  (* Filter with rebuilding via add:
     - we rebuild the set from scratch to keep invariants explicit
     - even if the original set has no duplicates, this keeps the style uniform *)
  let filter (p : elt -> bool) (s : t) : t =
    fold_left (fun acc x -> if p x then add x acc else acc) empty s

  (* Map may create duplicates, so we must re-add through add:
     - apply g to each element
     - insert into a new set with add to enforce uniqueness *)
  let map (g : elt -> elt) (s : t) : t =
    fold_left (fun acc x -> add (g x) acc) empty s

  (* Union of two sets:
     - fold over the second set, inserting into the first via add
     - add already handles duplicates *)
  let union (a : t) (b : t) : t = fold_left (fun acc x -> add x acc) a b

  (* subset a b: every element of a is in b *)
  let subset (a : t) (b : t) : bool = List.for_all (fun x -> mem x b) a

  (* equal_set a b:
     - same cardinality
     - each is subset of the other
     - since add/remove preserve uniqueness, this is a valid equality check *)
  let equal_set (a : t) (b : t) : bool = cardinal a = cardinal b && subset a b

  (* Convert to list:
     - just return the internal representation
     - the order is not specified in the abstract interface *)
  let to_list (s : t) : elt list = s

  (* Build a set from a list by inserting each element via add:
     - add will automatically remove duplicates *)
  let of_list (xs : elt list) : t =
    List.fold_left (fun acc x -> add x acc) empty xs
end
