(* json -> domain types *)

open Yojson.Basic.Util
open Yojson.Basic

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

  let member_opt (k : string) (j : t) : t option =
  match member k j with
  | `Null -> None
  | v -> Some v

let to_int_opt (j : t) : int option =
  try Some (to_int j) with _ -> None

let to_string_opt (j : t) : string option =
  match j with
  | `String s -> Some s
  | _ -> None

  let to_string_flex_opt (j : Yojson.Basic.t) : string option =
  match j with
  | `String s ->
      let s = String.trim s in
      if s = "" || String.lowercase_ascii s = "null" then None else Some s
  | `Null -> None
  | _ -> None

let to_list_opt (j : t) : t list option =
  match j with
  | `List xs -> Some xs
  | _ -> None


let int_of_string_opt (s : string) : int option =
  try Some (int_of_string s) with _ -> None

let to_int_flex_opt (j : Yojson.Basic.t) : int option =
  match j with
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)         (* optional: only if you want to accept floats *)
  | `String s ->
      let s = String.trim s in
      if s = "" || String.lowercase_ascii s = "null" then None
      else int_of_string_opt s
  | `Null -> None
  | _ -> None

let get_int (k : string) (j : t) : int =
  match member k j with
  | `Null ->
      failwith (Printf.sprintf "JSON field '%s' is null (expected int)" k)
  | v -> to_int v

let get_int_opt (k : string) (j : t) : int option =
  match member k j with
  | `Null -> None
  | v ->
      (try Some (to_int v) with _ -> None)

let get_string (k : string) (j : t) : string =
  j |> member k |> to_string

let get_string_opt (k : string) (j : Yojson.Basic.t) : string option =
  match member_opt k j with
  | None -> None
  | Some v -> to_string_flex_opt v
