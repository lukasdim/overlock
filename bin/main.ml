open Yojson.Basic.Util

let () =
  let player = Overlock.Update.Steamapi.get_playerdata (Overlock.Domain.Types.SteamId "159342403") in 
  let last_kda_haze = Overlock.Domain.Types.HeroMap.find Overlock.Deadlock.Types.Bebop player.all_kda in
  let k, d, a = Overlock.Domain.Calculation.sum_kda_list_first_n 10 last_kda_haze in
  Printf.printf "Summed: k=%d; d=%d; a=%d" k d a

(* Tests will be saved here as comments to note usage*)
let () =
  let player = Overlock.Update.Steamapi.get_playerdata (Overlock.Domain.Types.SteamId "159342403") in 
  let descriptions = Overlock.Domain.Calculation.calculate_descriptions player Overlock.Deadlock.Types.Bebop in
  let print_playerDescriptors =
  descriptions
  |> List.map Overlock.Transport.Mapper.string_of_playerDescriptor
  |> String.concat ", "
  in
  print_endline print_playerDescriptors