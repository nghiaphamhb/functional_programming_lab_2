# Лабораторная работа 2
## Вариант `oa-set`

- Студент: Фам Данг Чунг Нгиа
- Группа: P3321
- ИСУ: 374806
- Функциональный язык: OCaml

## Требования

Интерфейс — `Set`, структура данных — `OpenAddress Hashmap`.

1. Функции:
    * [x] добавление и удаление элементов;
    * [x] фильтрация;
    * [x] отображение (`map`);
    * [x] свертки (левая и правая);
    * [x] структура должна быть моноидом.
2. Структуры данных должны быть **неизменяемыми**.
3. Библиотека должна быть протестирована в рамках **unit testing**.
4. Библиотека должна быть протестирована в рамках **property-based** тестирования (*как минимум 3 свойства*, включая свойства моноида).
5. Структура должна быть **полиморфной**.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка — необходимо реализовать их вручную и по возможности — обеспечить совместимость.
7. Обратите внимание:
* API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
* Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

---

## Ключевые элементы реализации

### Интерфейс множества (Set API + Hashable)
```ocaml
(* in lib/oa_set.mli *)
(** интейфейс *)

(* тип данных элемента в Open Addressing Set *)
type 'a open_addressing_set =
  | Empty (* свободный *)
  | Occupied of 'a (* занятый *)
  | Deleted
(* удаленый *)

(* основные функции Set *)

val create_set : int -> 'a open_addressing_set list
(** [create_set size] создать пустой Set с определенным размером. *)

val insert : 'a open_addressing_set list -> 'a -> 'a open_addressing_set list
(** [insert set x] добавить элемент [x] в набор [set]. Если элемент уже
    существует, не добавляйте его снова. *)

val remove : 'a open_addressing_set list -> 'a -> 'a open_addressing_set list
(** [remove set x] удалить элемент [x] из набора [set]. *)

val contains : 'a open_addressing_set list -> 'a -> bool
(** [contains set x] проверить, находится ли элемент [x] в наборе [set]. *)

val hash_fn : 'a -> int -> int
(** [hash_fn x size] вычислить хэш-индекс элемента [x] в наборе с размером
    [size]. *)

val equal_set :
  'a open_addressing_set list -> 'a open_addressing_set list -> bool
(** [equal_set set set] сравнить два набора [set] и [set] *)

type 'a monoid_set = {
  empty : 'a open_addressing_set list;  (** нейтральный элемент = пустой сет *)
  op :
    'a open_addressing_set list ->
    'a open_addressing_set list ->
    'a open_addressing_set list;
      (** оператор = объединение сета *)
}
(** представитель моноида Open Addressing Set. *)

(** функции по свойству моноида *)

val monoid_set_op :
  'a open_addressing_set list ->
  'a open_addressing_set list ->
  'a open_addressing_set list
(** [monoid_set_op set1 set2] объединить два набора [set1] и [set2] в
    соответствии с бинарной операцией моноида. *)

val monoid_set_empty : int -> 'a open_addressing_set list
(** [monoid_set_empty size] создать пустой набор с размером [size], используемый
    в качестве единичного элемента в моноиде. *)

(** допольнительные функции *)

val filter :
  'a open_addressing_set list -> ('a -> bool) -> 'a open_addressing_set list
(** [filter set f] фильтрует элементы в наборе на основе функции условия [f]. *)

val map :
  'a open_addressing_set list -> ('a -> 'b) -> 'b open_addressing_set list
(** [map set f] применять функцию [f] к каждому элементу в наборе и верните
    новый набор. *)

val fold_left : 'a open_addressing_set list -> ('b -> 'a -> 'b) -> 'b -> 'b
(** [fold_left set f init] сложить элементы в наборе слева направо, начиная со
    значения [init]. *)

val fold_right : 'a open_addressing_set list -> ('a -> 'b -> 'b) -> 'b -> 'b
(** [fold_right set f init] сложить элементы в наборе справа налево, начиная со
    значения [init]. *)

```

### Добавление и удаление (add / remove)
```ocaml
(*  добавить элемент в набор *)
let insert (set : 'a open_addressing_set list) (x : 'a) :
    'a open_addressing_set list =
  let size = List.length set in
  let idx = hash_fn x size in

  let rec insert_helper set idx =
    match List.nth_opt set idx with
    | None -> set (* стоп в конце списка *)
    | Some Empty ->
        List.mapi (fun i el -> if i = idx then Occupied x else el) set
    | Some (Occupied v) when v = x ->
        set (* не добавляйте дублированный элемент *)
    | Some (Occupied _) | Some Deleted ->
        (* пропустить занятые, влияние удаления*)
        let next_idx = (idx + 1) mod size in
        insert_helper set next_idx
  in
  insert_helper set idx

(* удалить элемент из набора *)
let remove (set : 'a open_addressing_set list) (x : 'a) :
    'a open_addressing_set list =
  let size = List.length set in
  let idx = hash_fn x size in

  let rec remove_helper set idx =
    match List.nth_opt set idx with
    | None -> set
    | Some Empty -> set
    | Some (Occupied v) when v = x ->
        List.mapi (fun i el -> if i = idx then Deleted else el) set
    | Some (Occupied _) | Some Deleted ->
        (* пропустить занятые, влияние удаления*)
        let next_idx = (idx + 1) mod size in
        remove_helper set next_idx
  in
  remove_helper set idx
```

### Свертки (левая и правая):

```ocaml
(* fold_left *)
let fold_left (set : 'a open_addressing_set list) (f : 'b -> 'a -> 'b)
    (init : 'b) : 'b =
  List.fold_left
    (fun acc el -> match el with Occupied v -> f acc v | _ -> acc)
    init set

(* fold_right *)
let fold_right (set : 'a open_addressing_set list) (f : 'a -> 'b -> 'b)
    (init : 'b) : 'b =
  List.fold_right
    (fun el acc -> match el with Occupied v -> f v acc | _ -> acc)
    set init
```

### Фильтрация:

```ocaml
(* фильтрация *)
let filter (set : 'a open_addressing_set list) (f : 'a -> bool) :
    'a open_addressing_set list =
  let size = List.length set in
  List.fold_left
    (fun acc el ->
      match el with
      | Occupied v when f v ->
          insert acc v (* добавить только уникальный элемент в набор *)
      | _ -> acc)
    (create_set size) set
```

### Отображение (`map`):

```ocaml
(* map *)
let map (set : 'a open_addressing_set list) (f : 'a -> 'b) :
    'b open_addressing_set list =
  let size = List.length set in
  List.fold_left
    (fun acc el ->
      match el with
      | Occupied v -> insert acc (f v) (* применять функцию f на элемент *)
      | _ -> acc)
    (create_set size) set

```



### Моноид и его свойства

```ocaml
(* Бинарная математика: объединение двух наборов *)
let monoid_set_op (set1 : 'a open_addressing_set list)
    (set2 : 'a open_addressing_set list) : 'a open_addressing_set list =
  List.fold_left
    (fun acc el ->
      match el with
      | Occupied v ->
          insert acc v (* добавить только уникальный элемент в набор *)
      | _ -> acc)
    set1 set2

(* нейтральный элемент :пустый набор *)
let monoid_set_empty (size : int) : 'a open_addressing_set list =
  create_set size

(* тип моноида *)
type 'a monoid_set = {
  empty : 'a open_addressing_set list;  (* нейтральный элемент *)
  op :
    'a open_addressing_set list ->
    'a open_addressing_set list ->
    'a open_addressing_set list;
      (** оператор (union) *)
}
```

### Сравнение множеств

```ocaml
(* сравнение наборов *)
let equal_set (set1 : 'a open_addressing_set list)
    (set2 : 'a open_addressing_set list) : bool =
  List.length set1 = List.length set2
  && List.for_all
       (fun x ->
         match x with
         | Occupied v -> contains set2 v
         | _ -> true (* пропустить Empty или Deleted элементы *))
       set1
```

## Тестирование

В рамках данной работы были применены два инструмента:

  * [Alcotest](https://opam.ocaml.org/packages/alcotest/) - для модульного тестирования (unit testing);
  * [QCheck](https://opam.ocaml.org/packages/qcheck/) - для тестирования свойств (property-based testing).

### Unit-тесты (Alcotest)

Проверяют работы основных операций набора:
* insert: элемент после вставки присутствует в множестве, повторная вставка не создаёт дубликатов;
* remove: удалённый элемент больше не содержится в множестве;

Проверяется моноидная операция объединения:
* последовательное объединение трёх множеств даёт ожидаемый результат;
* объединение с пустым множеством не изменяет исходный set;

Проверяют работы допольнительных операций набора:
* filter: в результате остаются только элементы, удовлетворяющие предикату;
* map: все элементы преобразуются заданной функцией, и результат содержит правильные значения;
* fold_left и fold_right: итоговое значение (например, сумма элементов) совпадает с ожидаемым.

### Property-тесты (QCheck)

| Свойство          | Проверяемое выражение       | Смысл                                             |
| ----------------- | --------------------------- | ------------------------------------------------- |
| **Monoid empty**  | `s ∪ ∅ = s `                | Наличие нейтрального элемента                     |
| **Associativity** | `(a ∪ b) ∪ c = a ∪ (b ∪ c)`| Ассоциативность объединения                       |
| **Idempotent**    | `add x (add x s) = add x s` | Добавление того же элемента не изменяет множество |


### Отчёт инструмента тестирования
```bash
Testing `Open Addressing Set Tests'.
This run has ID `O7KT7VQK'.

  [OK]          Insert                           0   Insert element.
  [OK]          Remove                           0   Remove element.
  [OK]          Monoid                           0   Monoid operation.
  [OK]          Monoid empty                     0   Monoid empty set.
  [OK]          Filter                           0   Filter elements.
  [OK]          Map                              0   Map elements.
  [OK]          Fold Left                        0   Fold Left.
  [OK]          Fold Right                       0   Fold Right.
  [OK]          Monoid empty prop                0   Monoid empty property.
  [OK]          Monoid associative prop          0   Monoid associative property.
  [OK]          Add idempotent                   0   Add idempotent property.

Full test results in `~/project/oa_set/_build/default/test/_build/_tests/Open Addressing Set Tests'.
Test Successful in 0.229s. 11 tests run.
```

## Выводы

В данной лабораторной работе была реализована структура данных **«Хеш-множество с открытой адресацией» (Open Address Hash Set)**.
Эта структура обеспечивает эффективное хранение и поиск уникальных элементов, используя метод открытой адресации и линейного пробирования для разрешения коллизий.

В реализации были использованы следующие приёмы функционального программирования:

* **Неизменяемость (immutability):** все операции над множеством возвращают новое значение структуры, не изменяя исходное.
* **Рекурсия и функции высшего порядка:** для обхода и преобразования элементов (`map`, `fold`, `filter`) использовались функции высшего порядка.
* **Использование метода открытой адресации:** для эффективного размещения элементов и обработки коллизий применялись битовые маски и степенные размеры таблицы (мощность — степень двойки).
* **Тестирование:** корректность реализации проверялась с помощью библиотек `Alcotest` (unit-тестирование) и `QCheck` (property-based тестирование), включая проверку свойств моноида (`ассоциативность`, `идемпотентность`, `наличие нейтрального элемента`).

Таким образом, в ходе лабораторной работы были закреплены навыки проектирования модульных и полиморфных структур данных на языке OCaml, а также освоены принципы функционального тестирования и реализации неизменяемых коллекций.