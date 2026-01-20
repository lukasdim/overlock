(* 

Easy creation of multiple types. 
Ex. http server creates player object from one function call (this!) instead of making kda and kills and deaths

*)

let match_hero (hero_id : int) : Deadlock.Types.hero = 
  match hero_id with
    | 1 -> Deadlock.Types.Infernus
    | 2 -> Deadlock.Types.Seven
    | 3 -> Deadlock.Types.Vindicta
    | 4 -> Deadlock.Types.Geist
    | 6 -> Deadlock.Types.Abrams
    | 7 -> Deadlock.Types.Wraith
    | 8 -> Deadlock.Types.McGinnis
    | 10 -> Deadlock.Types.Paradox
    | 11 -> Deadlock.Types.Dynamo
    | 12 -> Deadlock.Types.Kelvin
    | 13 -> Deadlock.Types.Haze
    | 14 -> Deadlock.Types.Holliday
    | 15 -> Deadlock.Types.Bebop
    | 16 -> Deadlock.Types.Calico
    | 17 -> Deadlock.Types.Talon
    | 18 -> Deadlock.Types.Mo
    | 19 -> Deadlock.Types.Shiv
    | 20 -> Deadlock.Types.Ivy
    | 21 -> Deadlock.Types.Kali (*Not available in-game yet, but may release very soon*)
    | 25 -> Deadlock.Types.Warden
    | 27 -> Deadlock.Types.Yamato
    | 31 -> Deadlock.Types.Lash
    | 35 -> Deadlock.Types.Viscous
    | 38 -> Deadlock.Types.Gunslinger (*Not available in-game yet, but may release very soon*)
    | 39 -> Deadlock.Types.Boss (*Not available in-game yet, but may release very soon*)
    | 47 -> Deadlock.Types.Tokamak (*Not available in-game yet, but may release very soon*)
    | 48 -> Deadlock.Types.Wrecker (*Not available in-game yet, but may release very soon*)
    | 49 -> Deadlock.Types.Rutger (*Not available in-game yet, but may release very soon*)
    | 50 -> Deadlock.Types.Pocket
    | 51 -> Deadlock.Types.Thumper (*Not available in-game yet, but may release very soon*)
    | 52 -> Deadlock.Types.Mirage
    | 53 -> Deadlock.Types.Fathom (*Not available in-game yet, but may release very soon*)
    | 54 -> Deadlock.Types.Cadence (*Not available in-game yet, but may release very soon*)
    | 56 -> Deadlock.Types.Bomber (*Not available in-game yet, but may release very soon*)
    | 57 -> Deadlock.Types.Shield  (*Not available in-game yet, but may release very soon*)
    | 58 -> Deadlock.Types.Vyper
    | 59 -> Deadlock.Types.Vandal (*Not available in-game yet, but may release very soon*)
    | 60 -> Deadlock.Types.Sinclair
    | 61 -> Deadlock.Types.Trapper (*Not available in-game yet, but may release very soon*)
    | 62 -> Deadlock.Types.Raven (*Not available in-game yet, but may release very soon*)
    | 63 -> Deadlock.Types.Mina
    | 64 -> Deadlock.Types.Drifter
    | 65 -> Deadlock.Types.Venator (*Not available in-game yet, but may release very soon*)
    | 66 -> Deadlock.Types.Victor
    | 67 -> Deadlock.Types.Paige
    | 68 -> Deadlock.Types.Boho (*Not available in-game yet, but may release very soon*)
    | 69 -> Deadlock.Types.Doorman
    | 70 -> Deadlock.Types.Skyrunner (*Not available in-game yet, but may release very soon*)
    | 71 -> Deadlock.Types.Swan (*Not available in-game yet, but may release very soon*)
    | 72 -> Deadlock.Types.Billy
    | 73 -> Deadlock.Types.Druid (*Not available in-game yet, but may release very soon*)
    | 74 -> Deadlock.Types.Graf (*Not available in-game yet, but may release very soon*)
    | 75 -> Deadlock.Types.Fortuna (*Not available in-game yet, but may release very soon*)
    | _ -> Deadlock.Types.Unknown (* Needed in case hero_id DNE? Should never happen *)

let create_kda (k : int) (d : int) (a : int) : Domain.Types.kda = 
  let kill_info : Domain.Types.kill_info = {kills = k; killed=None} in
  let death_info : Domain.Types.death_info = {deaths = d; killers=None} in
  let kda : Domain.Types.kda = {kills = kill_info; deaths = death_info; assists = a} in
  kda