open Alcotest
open QCheck
open Oa_set

(* Unit Testing with Alcotest             *)

(* Тест insert *)
let test_insert () =
  let set = create_set 10 in
  let set = insert set 5 in
  let set = insert set 10 in
  (* Проверьте элемент в наборе *)
  let test_5 = contains set 5 in
  let test_10 = contains set 10 in
  let test_15 = contains set 15 in
  check Alcotest.bool "Test insert 5" true test_5;
  check Alcotest.bool "Test insert 10" true test_10;
  check Alcotest.bool "Test insert 15" false test_15

(* Тест remove *)
let test_remove () =
  let set = create_set 10 in
  let set = insert set 5 in
  let set = remove set 5 in
  (* Убедитесь, что элемент 5 был удален из набора *)
  let test_5 = contains set 5 in
  check Alcotest.bool "Test remove" false test_5

(* Тест monoid (оператор - объединение множеств) *)
let test_monoid_op () =
  let set1 = insert (create_set 10) 5 in
  let set2 = insert (create_set 10) 10 in
  let set3 = insert (create_set 10) 15 in
  let merged_set = monoid_set_op set1 set2 in
  let merged_set = monoid_set_op merged_set set3 in
  check Alcotest.bool "Test monoid op 5" true (contains merged_set 5);
  check Alcotest.bool "Test monoid op 10" true (contains merged_set 10);
  check Alcotest.bool "Test monoid op 15" true (contains merged_set 15)

(* Тест monoid (нейтральный элемент - пустое множество) *)
let test_monoid_empty () =
  let set1 = insert (create_set 10) 5 in
  let empty_set = monoid_set_empty 10 in
  let result_set = monoid_set_op set1 empty_set in
  check Alcotest.bool "Test monoid empty" true (contains result_set 5)

(* Тест фильтрации *)
let test_filter () =
  let set = create_set 10 in
  let set = insert set 5 in
  let set = insert set 10 in
  let set = insert set 15 in
  let filter_set = filter set (fun x -> x mod 2 = 0) in
  (* Убедитесь, что набор после фильтрации содержит только четные элементы *)
  check Alcotest.bool "Test filter 5" false (contains filter_set 5);
  check Alcotest.bool "Test filter 10" true (contains filter_set 10);
  check Alcotest.bool "Test filter 15" false (contains filter_set 15)

(* Тест map *)
let test_map () =
  let set = create_set 10 in
  let set = insert set 5 in
  let set = insert set 10 in
  let map_set = map set (fun x -> x * 2) in
  (* Убедитесь, что в новом наборе есть элементы, которые были продублированы *)
  check Alcotest.bool "Test map 5" true (contains map_set 10);
  check Alcotest.bool "Test map 10" true (contains map_set 20);
  check Alcotest.bool "Test map 15" false (contains map_set 15)

(* Тест fold *)
let test_fold_left () =
  let set = create_set 10 in
  let set = insert set 1 in
  let set = insert set 2 in
  let set = insert set 3 in
  let sum = fold_left set (fun acc x -> acc + x) 0 in
  (* Убедитесь, что fold_left вычисляет правильную сумму *)
  check Alcotest.int "Test fold_left" 6 sum

let test_fold_right () =
  let set = create_set 10 in
  let set = insert set 1 in
  let set = insert set 2 in
  let set = insert set 3 in
  let sum = fold_right set (fun x acc -> acc + x) 0 in
  (* Убедитесь, что fold_right вычисляет правильную сумму *)
  check Alcotest.int "Test fold_right" 6 sum

(* Property-based Testing with QCheck *)

(* Random generator 'a open_addressing_set *)
let open_addressing_set_elem_gen =
  let open Gen in
  frequency
    [
      (1, return Empty); (2, map (fun x -> Occupied x) int); (1, return Deleted);
    ]

(* QCheck.make needs arbitrary *)
let open_addressing_set_arbitrary = QCheck.make open_addressing_set_elem_gen

(* Property: Объединение с нейтральным элементом не изменяет исходный набор *)
let monoid_empty_property =
  let gen = open_addressing_set_arbitrary in
  let prop el =
    let empty_set = monoid_set_empty 10 in
    let result_set = monoid_set_op [ el ] empty_set in
    match el with Occupied v -> contains result_set v | _ -> true
  in
  QCheck.Test.make ~name:"Monoid empty set" gen prop

(* Property: Ассоциативность: (A ∪ B) ∪ C = A ∪ (B ∪ C) *)
let monoid_associative_property =
  let gen = open_addressing_set_arbitrary in
  let prop el =
    let set1 = insert (create_set 10) el in
    let set2 = insert (create_set 10) el in
    let set3 = insert (create_set 10) el in
    let result1 = monoid_set_op (monoid_set_op set1 set2) set3 in
    let result2 = monoid_set_op set1 (monoid_set_op set2 set3) in
    result1 = result2
  in
  QCheck.Test.make ~name:"Monoid associative" gen prop

(* Property: idempotent
После добавления дублированного элемента el убедитесь, что значения set1 и set2 равны *)
let add_idempotent_property =
  let gen = open_addressing_set_arbitrary in
  let prop el =
    let set1 = insert (create_set 10) el in
    let set2 = insert set1 el in
    equal_set set1 set2
  in
  QCheck.Test.make ~name:"add_idempotent" gen prop

let check_prop (t : QCheck.Test.t) () = QCheck.Test.check_exn t

(* Запускать тесты *)
let () =
  let open Alcotest in
  run "Open Addressing Set Tests"
    [
      (* Unit Tests *)
      ("Insert", [ test_case "Insert element" `Quick test_insert ]);
      ("Remove", [ test_case "Remove element" `Quick test_remove ]);
      ("Monoid", [ test_case "Monoid operation" `Quick test_monoid_op ]);
      ("Monoid empty", [ test_case "Monoid empty set" `Quick test_monoid_empty ]);
      (* Additional Unit Tests *)
      ("Filter", [ test_case "Filter elements" `Quick test_filter ]);
      ("Map", [ test_case "Map elements" `Quick test_map ]);
      ("Fold Left", [ test_case "Fold Left" `Quick test_fold_left ]);
      ("Fold Right", [ test_case "Fold Right" `Quick test_fold_right ]);
      (* Property Tests *)
      ( "Monoid empty prop",
        [
          test_case "Monoid empty property" `Quick
            (check_prop monoid_empty_property);
        ] );
      ( "Monoid associative prop",
        [
          test_case "Monoid associative property" `Quick
            (check_prop monoid_associative_property);
        ] );
      ( "Add idempotent",
        [
          test_case "Add idempotent property" `Quick
            (check_prop add_idempotent_property);
        ] );
    ]
