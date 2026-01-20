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
  | Unkillable (* Low Deaths *)

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

(*
MATCH TYPES
*)

type matchStatus =
  | Ongoing
  | Victory
  | Defeat

type matchData = {
  players : player list;
  status : matchStatus;
  start_time : int
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
  (*all_matches : matchData; remove for now to just get testing.*) 
}

let create_playerdata s age kda = { steamid = s; age = age; all_kda = kda }