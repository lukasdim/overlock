(* 

Easy creation of multiple types. 
Ex. http server creates player object from one function call (this!) instead of making kda and kills and deaths

*)
open Yojson.Basic
open Yojson.Basic.Util
open Domain.Json
open Domain.Types
open Deadlock.Types

let match_hero (hero_id : int) : hero = 
  match hero_id with
    | 1 -> Infernus
    | 2 -> Seven
    | 3 -> Vindicta
    | 4 -> Geist
    | 6 -> Abrams
    | 7 -> Wraith
    | 8 -> McGinnis
    | 10 -> Paradox
    | 11 -> Dynamo
    | 12 -> Kelvin
    | 13 -> Haze
    | 14 -> Holliday
    | 15 -> Bebop
    | 16 -> Calico
    | 17 -> Talon
    | 18 -> Mo
    | 19 -> Shiv
    | 20 -> Ivy
    | 21 -> Kali (*Not available in-game yet, but may release very soon*)
    | 25 -> Warden
    | 27 -> Yamato
    | 31 -> Lash
    | 35 -> Viscous
    | 38 -> Gunslinger (*Not available in-game yet, but may release very soon*)
    | 39 -> Boss (*Not available in-game yet, but may release very soon*)
    | 47 -> Tokamak (*Not available in-game yet, but may release very soon*)
    | 48 -> Wrecker (*Not available in-game yet, but may release very soon*)
    | 49 -> Rutger (*Not available in-game yet, but may release very soon*)
    | 50 -> Pocket
    | 51 -> Thumper (*Not available in-game yet, but may release very soon*)
    | 52 -> Mirage
    | 53 -> Fathom (*Not available in-game yet, but may release very soon*)
    | 54 -> Cadence (*Not available in-game yet, but may release very soon*)
    | 56 -> Bomber (*Not available in-game yet, but may release very soon*)
    | 57 -> Shield  (*Not available in-game yet, but may release very soon*)
    | 58 -> Vyper
    | 59 -> Vandal (*Not available in-game yet, but may release very soon*)
    | 60 -> Sinclair
    | 61 -> Trapper (*Not available in-game yet, but may release very soon*)
    | 62 -> Raven (*Not available in-game yet, but may release very soon*)
    | 63 -> Mina
    | 64 -> Drifter
    | 65 -> Venator (*Not available in-game yet, but may release very soon*)
    | 66 -> Victor
    | 67 -> Paige
    | 68 -> Boho (*Not available in-game yet, but may release very soon*)
    | 69 -> Doorman
    | 70 -> Skyrunner (*Not available in-game yet, but may release very soon*)
    | 71 -> Swan (*Not available in-game yet, but may release very soon*)
    | 72 -> Billy
    | 73 -> Druid (*Not available in-game yet, but may release very soon*)
    | 74 -> Graf (*Not available in-game yet, but may release very soon*)
    | 75 -> Fortuna (*Not available in-game yet, but may release very soon*)
    | _ -> Unknown (* Needed in case hero_id DNE? Should never happen *)

let string_of_playerDescriptor = function
  | Carry -> "Carry"
  | First -> "First"
  | Shy -> "Shy"
  | Support -> "Support"
  | Feeder -> "Feeder"
  | Unkillable -> "Unkillable"
  | LowDeaths -> "LowDeaths"

let matchdata_of_json (j : Yojson.Basic.t) : matchData =
  {
    hero_id = get_int "hero_id" j;
    hero_level = get_int "hero_level" j;
    game_mode = get_int "game_mode" j;
    match_id = get_int "match_id" j;
    player_team = get_int "player_team" j;
    player_kills = get_int "player_kills" j;
    player_deaths = get_int "player_deaths" j;
    player_assists = get_int "player_assists" j;
    denies = get_int "denies" j;
    abandoned_time_s = get_int_opt "abandoned_time" j;
    match_duration_s = get_int_opt "duration" j;
    match_result = get_int "match_result" j;
    objectives_mask_team0 = get_int "objectives_mask_team0" j;
    objectives_mask_team1 = get_int "objectives_mask_team1" j;
    brawl_score_team0 = get_int_opt "brawl_score_team0" j;
    brawl_score_team1 = get_int_opt "brawl_score_team1" j;
    brawl_avg_round_time_s = get_int_opt "brawl_avg_round_time_s" j;
    last_hits = get_int "last_hits" j;
    net_worth = get_int "net_worth" j;   (* souls = net_worth*)
    start_time = get_int "start_time" j;
  }

let create_kda (k : int) (d : int) (a : int) : Domain.Types.kda = 
  let kill_info : Domain.Types.kill_info = {kills = k; killed=None} in
  let death_info : Domain.Types.death_info = {deaths = d; killers=None} in
  let kda : Domain.Types.kda = {kills = kill_info; deaths = death_info; assists = a} in
  kda

let match_list_of_response (json : Yojson.Basic.t) : Yojson.Basic.t list =
  match json with
  | `List xs -> xs
  | _ -> []


let map_match_data (json : Yojson.Basic.t) : Domain.Types.matchData list =
  json
  |> match_list_of_response
  |> List.map matchdata_of_json