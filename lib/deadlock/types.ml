(* 
=== In-game types ===
*)

type hero =
  | Infernus
  | Seven
  | Vindicta
  | Geist
  | Abrams
  | Wraith
  | McGinnis
  | Paradox
  | Dynamo
  | Kelvin
  | Haze
  | Holliday
  | Bebop
  | Calico
  | Talon
  | Mo
  | Shiv
  | Ivy
  | Kali (*Not available in-game yet, but may release very soon*)
  | Warden
  | Yamato
  | Lash
  | Viscous
  | Gunslinger (*Not available in-game yet, but may release very soon*)
  | Boss (*Not available in-game yet, but may release very soon*)
  | Tokamak (*Not available in-game yet, but may release very soon*)
  | Wrecker (*Not available in-game yet, but may release very soon*)
  | Rutger (*Not available in-game yet, but may release very soon*)
  | Pocket
  | Thumper (*Not available in-game yet, but may release very soon*)
  | Mirage
  | Fathom (*Not available in-game yet, but may release very soon*)
  | Cadence (*Not available in-game yet, but may release very soon*)
  | Bomber (*Not available in-game yet, but may release very soon*)
  | Shield (*Not available in-game yet, but may release very soon*)
  | Vyper
  | Vandal (*Not available in-game yet, but may release very soon*)
  | Sinclair
  | Trapper (*Not available in-game yet, but may release very soon*)
  | Raven (*Not available in-game yet, but may release very soon*)
  | Mina
  | Drifter
  | Venator (*Not available in-game yet, but may release very soon*)
  | Victor
  | Paige
  | Boho (*Not available in-game yet, but may release very soon*)
  | Doorman
  | Skyrunner (*Not available in-game yet, but may release very soon*)
  | Swan (*Not available in-game yet, but may release very soon*)
  | Billy
  | Druid (*Not available in-game yet, but may release very soon*)
  | Graf (*Not available in-game yet, but may release very soon*)
  | Fortuna (*Not available in-game yet, but may release very soon*)
  | Unknown (* Needed in case hero_id DNE? Should never happen *)

type rank =
  | Ascendant
  | Oracle

type vitality_item =
  | Spellbreaker

type gun_item =
  | TitanicMag

type item = 
  | Vitality of vitality_item
  | Gun of gun_item