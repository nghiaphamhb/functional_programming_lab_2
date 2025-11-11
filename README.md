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

### Добавление и удаление (add / remove, иммутабельность)
```ocaml
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
          add x (rehash (capacity s lsl 1) s)

let remove x s =
    match find_slot s.tbl x with
    | `Found idx ->
        let tbl' = Array.copy s.tbl in
        tbl'.(idx) <- Tombstone;
        let s' = { tbl = tbl'; size = s.size - 1; used = s.used } in
        if
          capacity s' > initial_capacity
          && float_of_int s'.size /. float_of_int (capacity s') < 0.2
        then rehash (capacity s' lsr 1) s'
        else s'
    | _ -> s
```

### Свертки (левая и правая):

```ocaml
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
```

### Фильтрация:

```ocaml
let filter p s =
    fold_left (fun acc x -> if p x then add x acc else acc) empty s
```

### Отображение (`map`):

```ocaml
let map g s =
    fold_left (fun acc x -> add (g x) acc) empty s

```



### Соответствие свойству моноида

```ocaml
let union a b =
    let a, b = if a.size >= b.size then (a, b) else (b, a) in
    fold_left (fun acc x -> add x acc) a b

let subset a b = fold_left (fun ok x -> ok && mem x b) true a
```

### Сравнение множеств

```ocaml
let equal_set a b =
    if a.size <> b.size then false
    else
      let small, big = if a.size <= b.size then (a, b) else (b, a) in
      fold_left (fun ok x -> ok && mem x big) true small

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
