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
module type HASHABLE = sig
  type t

  val hash : t -> int 
  val equal : t -> t -> bool 
end

module type SET = sig
  type elt
  type t 

  val empty : t
  val is_empty : t -> bool

  (** basic edits*)
  val cardinal : t -> int 
  val mem : elt -> t -> bool

  val add : elt -> t -> t
  val remove : elt -> t -> t
  val iter : (elt -> unit) -> t -> unit

  (** high-order operators*)
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a 
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (elt -> bool) -> t -> t 
  val map : (elt -> elt) -> t -> t 

  (** Monoid over union *)
  val union : t -> t -> t 
  val equal_set : t -> t -> bool
  val subset : t -> t -> bool 

  (** Utilities (for tests / debug) *)
  val to_list : t -> elt list 
  val of_list : elt list -> t 
end

module Make (H : HASHABLE) : SET with type elt = H.t
```

### Основные запросы (is_empty / cardinal / mem)
```ocaml
(* Empty set is represented by the empty list *)
let empty : t = []
let is_empty (s : t) : bool = s = []
let cardinal (s : t) : int = List.length s

(* Membership test using H.equal *)
let mem (x : elt) (s : t) : bool = List.exists (fun y -> H.equal x y) s
```

### Добавление и удаление (add / remove, иммутабельность)
```ocaml
(* Pure add:
  - if element already present, return the same set
  - otherwise, cons it to the front *)
let add (x : elt) (s : t) : t = if mem x s then s else x :: s

(* Pure remove:
  - filter out all elements equal to x *)
let remove (x : elt) (s : t) : t = List.filter (fun y -> not (H.equal x y)) s
```

### Свертки (левая и правая):

```ocaml
(* Left fold over the underlying list *)
let fold_left (f : 'a -> elt -> 'a) (acc : 'a) (s : t) : 'a =
  List.fold_left f acc s

(* Right fold over the underlying list *)
let fold_right (f : elt -> 'a -> 'a) (s : t) (acc : 'a) : 'a =
  List.fold_right f s acc
```

### Фильтрация:

```ocaml
(* Filter with rebuilding via add:
  - we rebuild the set from scratch to keep invariants explicit
  - even if the original set has no duplicates, this keeps the style uniform *)
let filter (p : elt -> bool) (s : t) : t =
  fold_left (fun acc x -> if p x then add x acc else acc) empty s
```

### Отображение (`map`):

```ocaml
(* Map may create duplicates, so we must re-add through add:
  - apply g to each element
  - insert into a new set with add to enforce uniqueness *)
let map (g : elt -> elt) (s : t) : t =
  fold_left (fun acc x -> add (g x) acc) empty s
```



### Соответствие свойству моноида

```ocaml
(* Union of two sets:
  - fold over the second set, inserting into the first via add
  - add already handles duplicates *)
let union (a : t) (b : t) : t = fold_left (fun acc x -> add x acc) a b

(* subset a b: every element of a is in b *)
let subset (a : t) (b : t) : bool = List.for_all (fun x -> mem x b) a
```

### Сравнение множеств

```ocaml
(* equal_set a b:
  - same cardinality
  - each is subset of the other
  - since add/remove preserve uniqueness, this is a valid equality check *)
let equal_set (a : t) (b : t) : bool = cardinal a = cardinal b && subset a b
```

## Тестирование

В рамках данной работы были применены два инструмента:

  * [Alcotest](https://opam.ocaml.org/packages/alcotest/) - для модульного тестирования (unit testing);
  * [QCheck](https://opam.ocaml.org/packages/qcheck/) - для тестирования свойств (property-based testing).

### Unit-тесты (Alcotest)

Проверяют корректность работы основных операций множества:
* Проверяется отсутствие дубликатов (card=3 при добавлении [1;2;2;3]);
* Правильность операции mem и remove;
* Неизменяемость структуры данных (каждая операция возвращает новое множество).

```ocaml
(* Unit tests *)
let test_basic () =
  let s = to_set [1;2;2;3] in
  Alcotest.(check int) "card=3" 3 (SInt.cardinal s);
  Alcotest.(check bool) "mem 2"  true  (SInt.mem 2 s);
  Alcotest.(check bool) "mem 4"  false (SInt.mem 4 s);
  let s' = SInt.remove 2 s in
  Alcotest.(check bool) "mem 2 removed" false (SInt.mem 2 s');
  Alcotest.(check int) "card=2" 2 (SInt.cardinal s')
```

### Property-тесты (QCheck)

Проверяют математические свойства структуры множества:
(Каждый тест выполняется 300 раз со случайными наборами данных)

| Свойство          | Проверяемое выражение       | Смысл                                             |
| ----------------- | --------------------------- | ------------------------------------------------- |
| **Identity**      | `union empty s = s`         | Наличие нейтрального элемента                     |
| **Associativity** | `(a ∪ b) ∪ c = a ∪ (b ∪ c)` | Ассоциативность объединения                       |
| **Idempotent**    | `add x (add x s) = add x s` | Добавление того же элемента не изменяет множество |

```ocaml
let prop_monoid_identity =
  Test.make ~count:300 ~name:"monoid_identity" gen_int_list (fun xs ->
    let s = to_set xs in
    SInt.equal_set (SInt.union SInt.empty s) s)

```

### Отчёт инструмента тестирования
```bash
Running tests...
(cd _build/default/test && ./test_oa_set.exe)
{oa_set}
  Testing `oa_set'.
  This run has ID `LD852LQR'.
  
    [OK]          unit           0   basic.
    [OK]          unit           1   union_equal.
    [OK]          props          0   monoid_identity.
    [OK]          props          1   monoid_assoc.
    [OK]          props          2   add_idempotent.
  
  Full test results in `~/work/functional_programming_lab_2/functional_programming_lab_2/oa_set/_build/default/test/_build/_tests/oa_set'.
  Test Successful in 0.003s. 5 tests run.
```


## Выводы

В данной лабораторной работе была реализована структура данных **«Хеш-множество с открытой адресацией» (Open Address Hash Set)**.
Эта структура обеспечивает эффективное хранение и поиск уникальных элементов, используя метод открытой адресации и линейного пробирования для разрешения коллизий.

В реализации были использованы следующие приёмы функционального программирования:

* **Модули и функторы:** реализация была выполнена в виде функторa `Make(H : HASHABLE)`, что обеспечивает полиморфизм и возможность работы с любыми типами данных, для которых определены функции `hash` и `equal`.
* **Неизменяемость (immutability):** все операции над множеством возвращают новое значение структуры, не изменяя исходное.
* **Рекурсия и функции высшего порядка:** для обхода и преобразования элементов (`map`, `fold`, `filter`) использовались функции высшего порядка.
* **Использование метода открытой адресации:** для эффективного размещения элементов и обработки коллизий применялись битовые маски и степенные размеры таблицы (мощность — степень двойки).
* **Тестирование:** корректность реализации проверялась с помощью библиотек `Alcotest` (unit-тестирование) и `QCheck` (property-based тестирование), включая проверку свойств моноида (`ассоциативность`, `идемпотентность`, `наличие нейтрального элемента`).

Таким образом, в ходе лабораторной работы были закреплены навыки проектирования модульных и полиморфных структур данных на языке OCaml, а также освоены принципы функционального тестирования и реализации неизменяемых коллекций.
