type 'a open_addressing_set = Empty | Occupied of 'a | Deleted

(* OCaml поддерживает хэш-функции Hashtbl.hash *)
let hash_fn (x : 'a) (size : int) : int =
  let hash_code = Hashtbl.hash x in
  hash_code mod size

(* создать пустой Set с определенным размером. *)
let create_set size = List.init size (fun _ -> Empty)

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

(* проверить, находится ли элемент в наборе *)
let contains (set : 'a open_addressing_set list) (x : 'a) : bool =
  let size = List.length set in
  let idx = hash_fn x size in

  let rec contains_helper set idx =
    match List.nth_opt set idx with
    | None -> false
    | Some Empty -> false
    | Some (Occupied v) when v = x -> true
    | Some (Occupied _) | Some Deleted ->
        let next_idx = (idx + 1) mod size in
        contains_helper set next_idx
  in
  contains_helper set idx

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
  empty : 'a open_addressing_set list; (* нейтральный элемент *)
  op :
    'a open_addressing_set list ->
    'a open_addressing_set list ->
    'a open_addressing_set list;
      (** оператор (union) *)
}

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
