let () =
  let age = Overlock.Update.Steamapi.get_playerdata (Overlock.Domain.Types.SteamId "123196157") in 
  let last_kda_haze = Overlock.Domain.Types.HeroMap.find Overlock.Deadlock.Types.Lash age.all_kda in
  let k, d, a = Overlock.Update.Steamapi.sum_kda_list_first_n 10 last_kda_haze in
  Printf.printf "Summed: k=%d; d=%d; a=%d" k d a