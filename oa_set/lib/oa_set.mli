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
