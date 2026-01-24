(* Calculates playerDescriptions and more *)
open Yojson.Basic.Util
open Types
open Json

let rec take n xs =
  match n, xs with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

let sum_kda_list_first_n (n : int) (xs : kda list) : int * int * int =
  xs
  |> take n
  |> List.fold_left
       (fun (k_sum, d_sum, a_sum) (k : kda) ->
          ( k_sum + k.kills.kills
          , d_sum + k.deaths.deaths
          , a_sum + k.assists ))
       (0, 0, 0)

let get_most_recent_game (playerData : playerData) : matchData option = 
  try
    match playerData.all_matches with
      | [] -> None
      | x :: playerData -> Some x
  with
  | Yojson.Json_error _ -> None

let is_today (ts : float) : bool =
  let today = Unix.localtime (Unix.time ()) in
  let t = Unix.localtime ts in
  t.tm_year = today.tm_year && t.tm_yday = today.tm_yday

let check_first_game (matchData : matchData option) : bool = 
    match matchData with
      | Some m -> m.start_time |> float_of_int |> is_today
      | None -> false

let calculate_descriptions
    (player : playerData)
    (hero : Deadlock.Types.hero)
  : Types.playerDescriptors list =

  let hero_kda_map =
    HeroMap.find hero player.all_kda
  in

  let k, d, a =
    sum_kda_list_first_n 10 hero_kda_map
  in

  let played_today = get_most_recent_game player |> check_first_game
  in

  let rules =
    [
      (k > 100, Carry); (* 15 avg per game for 10 games is 150. same with other conditions below*)
      (k < 40 && a > 100, Support);
      (d <= 30,  Unkillable);
      (d <= 50 && d > 30,  LowDeaths);
      (k < 50 && d < 50,  Shy);
      (d >= 100,  Feeder);
      (not played_today, First)
    ]
  in

  rules
  |> List.filter_map (fun (cond, desc) ->
       if cond then Some desc else None
     )