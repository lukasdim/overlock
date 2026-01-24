type steamid3 = SteamId of string

let string_of_steamid64 = function
  | SteamId s -> s

(*
=== Player types ===
*)

type playerDescriptors =
  | Carry (* High Kills *)
  | First (* First game of the day *)
  | Shy (* Low Deaths, Low Kills*)
  | Support (* Low kills, High Assists*)
  | Feeder (* High Deaths*)
  | Unkillable (* Extremely Low Deaths *)
  | LowDeaths (* Low Deaths*)

type kill_info = {
  kills : int;
  killed : steamid3 list option (* Only used realtime *)
}

type death_info = {
  deaths : int;
  killers : steamid3 list option (* Only used realtime *)
}

type kda = {
  kills : kill_info;
  deaths : death_info;
  assists : int;
}

let create_kda k d a = { kills = k; deaths = d; assists = a }

type player = {
  steamid : steamid3;
  name : string;
  rank : Deadlock.Types.rank option; (* May be null, not needed for showing lists of matches. *)
  hero : Deadlock.Types.hero option; (* May be null, if searching outside of a game *)
  description : playerDescriptors list option; (* May be null, if searching outside of a game or player doesn't fit any *)
  kda : kda option; (* May be null, if searching outside of a game *)
  items : Deadlock.Types.item list option; (* May be null, at beginning of games and if searching outside of a game*)
}

let create_player s n r h d k i= { steamid = s; name = n; rank = r; hero = h; description = d;kda = k; items=i }

type matchData = {
  hero_id : int; (* Transport.Mapper *)
  hero_level : int;
  game_mode : int; (* 1 = Normal 6v6 , 4 = Brawl gamemode*)
  match_id : int;
  player_team : int; (* 0 or 1*)
  player_kills : int; (* use this instead of kda because kda is used for realtime updates mid-game. this is just history *)
  player_deaths : int;
  player_assists : int;
  denies : int; (* # of times stole souls from enemy minions *)
  abandoned_time_s : int option; (* # Seconds into game if abandoned. Maybe useful sometime in the future. sometimes null or 0, check both *)
  match_duration_s : int option;(* # Seconds of game duration *)
  match_result : int; (* 0 or 1. Not sure which is win and which is lose. *)
  objectives_mask_team0 : int; (* ??, always returns 65093 if it's a brawl match *)
  objectives_mask_team1 : int; (* ??, always returns 65093 if it's a brawl match *)
  brawl_score_team0 : int option; (* null if not in brawl*)
  brawl_score_team1 : int option; (* null if not in brawl*)
  brawl_avg_round_time_s : int option;
  last_hits: int; (* ?? *)
  net_worth: int; (* souls total *)
  start_time : int; (* Unix timestamp *)
}

module Hero = struct
  type t = Deadlock.Types.hero
  let compare a b =
    Stdlib.compare a b
end

(* will be used as hero -> kda list*)
module HeroMap = Map.Make(Hero);;

let add_kda (hero : Deadlock.Types.hero) (kda : kda) (m : kda list HeroMap.t)
  : kda list HeroMap.t =
  HeroMap.update hero (function
    | None -> Some [kda]
    | Some xs -> Some (kda :: xs)
  ) m

let build_kda_map (heroes : Deadlock.Types.hero list) (kdas : kda list)
  : kda list HeroMap.t =
  let m =
    List.fold_left2
      (fun acc hero kda -> add_kda hero kda acc)
      HeroMap.empty
      heroes
      kdas
  in
  (* optional: reverse lists so match order is preserved *)
  HeroMap.map List.rev m

type playerData = {
  steamid : steamid3;
  age : int; (* Unix timestamp of their first match *)
  all_kda : kda list HeroMap.t; (* Seperated for easy playerDescriptors Calculations*) 
  all_matches : matchData list;
}

let create_playerdata s age kda m = { steamid = s; age = age; all_kda = kda; all_matches = m }