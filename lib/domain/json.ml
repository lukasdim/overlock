(* json -> domain types *)

open Yojson.Basic.Util

(* List.nth throws if out of bounds if not checked *)
let nth_opt i xs =
  if i < 0 || i >= List.length xs then None else Some (List.nth xs i)

let last_opt = function
  | [] -> None
  | x :: xs ->
      let rec aux curr = function
        | [] -> Some curr
        | y :: ys -> aux y ys
      in
      aux x xs

let convert_to_json (json : string) : Yojson.Basic.t =
  let json = Yojson.Basic.from_string json in
    json

let get_json_ints (value : string) (json : Yojson.Basic.t) : int list =
  [json]
    |> flatten
    |> filter_member value
    |> filter_int
  
let get_json_bools (value : string) (json : Yojson.Basic.t) : bool list =
  [json]
    |> flatten
    |> filter_member value
    |> filter_bool

let get_json_strings (value : string) (json : Yojson.Basic.t) : string list =
  [json]
    |> flatten
    |> filter_member value
    |> filter_string

let get_json_value_at_index (list : 'a list) (index : int) : 'a option =
  nth_opt index list