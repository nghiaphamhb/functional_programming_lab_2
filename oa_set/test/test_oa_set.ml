open Oa_set

(* ===== Hashable int & Set int ===== *)

module HInt = struct
  type t = int

  let hash x = x
  let equal = Int.equal
end

module SInt = Make (HInt)

let to_set xs = List.fold_left (fun acc x -> SInt.add x acc) SInt.empty xs

(* ===== Unit tests (Alcotest) ===== *)

let test_basic () =
  let s = to_set [ 1; 2; 2; 3 ] in
  Alcotest.(check int) "card=3" 3 (SInt.cardinal s);
  Alcotest.(check bool) "mem 2" true (SInt.mem 2 s);
  Alcotest.(check bool) "mem 4" false (SInt.mem 4 s);
  let s' = SInt.remove 2 s in
  Alcotest.(check bool) "mem 2 removed" false (SInt.mem 2 s');
  Alcotest.(check int) "card=2" 2 (SInt.cardinal s')

let test_union_equal () =
  let a = to_set [ 1; 2; 3 ] in
  let b = to_set [ 3; 4 ] in
  let u1 = SInt.union (SInt.union a b) (to_set [ 5 ]) in
  let u2 = SInt.union a (SInt.union b (to_set [ 5 ])) in
  Alcotest.(check bool) "assoc" true (SInt.equal_set u1 u2);
  Alcotest.(check bool)
    "identity" true
    (SInt.equal_set (SInt.union SInt.empty a) a)

(* ===== Property tests (QCheck + Alcotest wrapper) ===== *)

open QCheck

let gen_int_list = small_list small_int

let prop_monoid_identity =
  Test.make ~count:300 ~name:"monoid_identity" gen_int_list (fun xs ->
      let s = to_set xs in
      SInt.equal_set (SInt.union SInt.empty s) s)

let prop_monoid_assoc =
  Test.make ~count:300 ~name:"monoid_assoc"
    (triple gen_int_list gen_int_list gen_int_list) (fun (xs, ys, zs) ->
      let a, b, c = (to_set xs, to_set ys, to_set zs) in
      let lhs = SInt.union (SInt.union a b) c in
      let rhs = SInt.union a (SInt.union b c) in
      SInt.equal_set lhs rhs)

let prop_add_idempotent =
  Test.make ~count:300 ~name:"add_idempotent" (pair gen_int_list small_int)
    (fun (xs, x) ->
      let s1 = SInt.add x (to_set xs) in
      let s2 = SInt.add x s1 in
      SInt.equal_set s1 s2)

(* helper: run 1 property likes 1 test-case Alcotest *)
let check_prop (t : QCheck.Test.t) () = QCheck.Test.check_exn t

let () =
  let open Alcotest in
  run "oa_set"
    [
      ( "unit",
        [
          test_case "basic" `Quick test_basic;
          test_case "union_equal" `Quick test_union_equal;
        ] );
      ( "props",
        [
          test_case "monoid_identity" `Quick (check_prop prop_monoid_identity);
          test_case "monoid_assoc" `Quick (check_prop prop_monoid_assoc);
          test_case "add_idempotent" `Quick (check_prop prop_add_idempotent);
        ] );
    ]
