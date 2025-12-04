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

module Make (H : HASHABLE) : SET with type elt = H.t = struct
  type elt = H.t
  type slot = Empty | Tombstone | Occupied of elt

  type t = {
    tbl : slot array; (* hash table storage *)
    size : int; (* number of Occupied *)
    used : int; (* Occupied + Tombstone *)
  }

  let initial_capacity = 8 (* must be power-of-two *)
  let empty = { tbl = Array.make initial_capacity Empty; size = 0; used = 0 }
  let is_empty s = s.size = 0
  let cardinal s = s.size
  let capacity s = Array.length s.tbl

  (* Ensure power-of-two capacity for cheap modulo via bitmask *)
  let is_pow2 n = n > 0 && n land (n - 1) = 0

  let mask i cap =
    (* if cap is power of two *)
    i land (cap - 1)

  let hash x = H.hash x land max_int

  (* Find either a matching slot or an insertion slot *)
  let find_slot tbl x =
    let cap = Array.length tbl in
    assert (is_pow2 cap);
    let h0 = mask (hash x) cap in
    let rec loop i first_tomb =
      if i = cap then
        match first_tomb with Some j -> `Insert j | None -> `Full
      else
        let idx = mask (h0 + i) cap in
        match tbl.(idx) with
        | Empty -> (
            (* not found, best insertion is first_tomb if present, else here *)
            match first_tomb with
            | Some j -> `Insert j
            | None -> `Insert idx)
        | Tombstone ->
            let ft = match first_tomb with None -> Some idx | s -> s in
            loop (i + 1) ft
        | Occupied y ->
            if H.equal x y then `Found idx else loop (i + 1) first_tomb
    in
    loop 0 None

  let mem x s = match find_slot s.tbl x with `Found _ -> true | _ -> false
  let load_factor_used s = float_of_int s.used /. float_of_int (capacity s)

  let next_pow2 n =
    let n = if n < 8 then 8 else n in
    let rec up k =
      (* multiply k by 2 until k >= n *)
      if k >= n then k else up (k lsl 1)
    in
    up 1

  let rehash new_cap s =
    let new_cap =
      let k = next_pow2 new_cap in
      if is_pow2 k then k else failwith "capacity must be pow2"
    in
    let tbl' = Array.make new_cap Empty in
    let cap' = Array.length tbl' in
    let insert_raw x =
      let h0 = mask (hash x) cap' in
      let rec loop i =
        let idx = mask (h0 + i) cap' in
        match tbl'.(idx) with
        | Empty -> tbl'.(idx) <- Occupied x
        | _ -> loop (i + 1)
      in
      loop 0
    in
    Array.iter (function Occupied x -> insert_raw x | _ -> ()) s.tbl;
    { tbl = tbl'; size = s.size; used = s.size }

  let ensure_room s =
    if load_factor_used s > 0.7 then rehash (capacity s lsl 1) s else s

  let rec add x s =
    if mem x s then s
    else
      let s = ensure_room s in
      let tbl' = Array.copy s.tbl in
      match find_slot tbl' x with
      | `Insert idx ->
          let used' =
            match tbl'.(idx) with
            | Empty -> s.used + 1
            | Tombstone -> s.used
            | Occupied _ -> s.used
          in
          tbl'.(idx) <- Occupied x;
          { tbl = tbl'; size = s.size + 1; used = used' }
      | `Found _ -> s
      | `Full ->
          (* extremely unlikely due to ensure_room; recover by rehash and retry *)
          add x (rehash (capacity s lsl 1) s)

  let remove x s =
    match find_slot s.tbl x with
    | `Found idx ->
        let tbl' = Array.copy s.tbl in
        tbl'.(idx) <- Tombstone;
        let s' = { tbl = tbl'; size = s.size - 1; used = s.used } in
        (* optional shrink if too sparse *)
        if
          capacity s' > initial_capacity
          && float_of_int s'.size /. float_of_int (capacity s') < 0.2
        then rehash (capacity s' lsr 1) s'
        else s'
    | _ -> s

  let iter f s = Array.iter (function Occupied x -> f x | _ -> ()) s.tbl

  let fold_left f acc s =
    let r = ref acc in
    iter (fun x -> r := f !r x) s;
    !r

  let fold_right f s acc =
    let r = ref acc in
    for i = Array.length s.tbl - 1 downto 0 do
      match s.tbl.(i) with Occupied x -> r := f x !r | _ -> ()
    done;
    !r

  let filter p s =
    (* rebuild cleanly to discard tombstones *)
    fold_left (fun acc x -> if p x then add x acc else acc) empty s

  let map g s =
    (* type-preserving: map elt->elt *)
    fold_left (fun acc x -> add (g x) acc) empty s

  let union a b =
    let a, b = if a.size >= b.size then (a, b) else (b, a) in
    (* copy-on-write: reuse [a], bulk-add elems of [b] *)
    fold_left (fun acc x -> add x acc) a b

  let equal_set a b =
    if a.size <> b.size then false
    else
      let small, big = if a.size <= b.size then (a, b) else (b, a) in
      fold_left (fun ok x -> ok && mem x big) true small

  let subset a b = fold_left (fun ok x -> ok && mem x b) true a
  let to_list s = fold_right (fun x acc -> x :: acc) s []
  let of_list xs = List.fold_left (fun acc x -> add x acc) empty xs
end