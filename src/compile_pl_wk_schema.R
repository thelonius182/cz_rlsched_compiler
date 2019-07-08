spoorboekje <- read_csv("spoorboekje_20190711.csv") %>%
  mutate(cur_cz_week_key = ymd_h(paste0(pgm_dtm, " ", pgm_start)))

pl_weekschema_uzm <- cur_cz_week_uzm %>% 
  left_join(spoorboekje, by = c("cz_tijdstip" = "cur_cz_week_key")) %>% 
  select(mac, pgm_dtm, pgm_start, cz_slot_len, titel_gids = title, 
         herh_van, sched_playlist, sys_audiotitel, cz_tijdstip) %>% 
  mutate(bijz = str_replace_all(string = sys_audiotitel, 
                                pattern = "\\d{4}-\\d{2}-\\d{2} \\w{4}\\.\\d{3}|,|Live \\(.*", 
                                replacement = ""),
         duur = str_pad(cz_slot_len, width = 3, side = "left", pad = "0"),
         sorteren = if_else(herh_van == "-", format(cz_tijdstip, format = "%Y%m%d_%H"), herh_van)
  ) %>%
  select(mac, duur, titel_gids, herh_van, hijack = bijz, sched_playlist, sorteren) %>% 
  arrange(desc(hijack), mac, sorteren) %>% 
  mutate(dubbel = if_else(lag(sorteren) == sorteren, 1, 0),
         dubbel = if_else(is.na(dubbel), 0, dubbel),
         replay = if_else(trimws(hijack) == "HiJack", hijack, if_else(herh_van == "-", "nieuw", "herhaling"))
  ) %>% 
  filter(dubbel == 0) %>% 
  select(-dubbel, mac, opzoekdatum = sorteren, titel_in_gids = titel_gids, type = replay, playlist = sched_playlist, duur) %>% 
  filter(playlist != "live > geen playlist nodig")

pl_weekschema_lgm <- cur_cz_week_lgm %>% 
  left_join(spoorboekje, by = c("cz_tijdstip" = "cur_cz_week_key")) %>% 
  select(mac, pgm_dtm, pgm_start, cz_slot_len, titel_gids = title, 
         herh_van, sched_playlist, sys_audiotitel, cz_tijdstip) %>% 
  mutate(bijz = str_replace_all(string = sys_audiotitel, 
                                pattern = "\\d{4}-\\d{2}-\\d{2} \\w{4}\\.\\d{3}|,|Live \\(.*", 
                                replacement = ""),
         duur = str_pad(cz_slot_len, width = 3, side = "left", pad = "0"),
         sorteren = if_else(herh_van == "-", format(cz_tijdstip, format = "%Y%m%d_%H"), herh_van)
  ) %>%
  select(mac, duur, titel_gids, herh_van, hijack = bijz, sched_playlist, sorteren) %>% 
  arrange(desc(hijack), mac, sorteren) %>% 
  mutate(dubbel = if_else(lag(sorteren) == sorteren, 1, 0),
         dubbel = if_else(is.na(dubbel), 0, dubbel),
         replay = if_else(trimws(hijack) == "HiJack", hijack, if_else(herh_van == "-", "nieuw", "herhaling"))
  ) %>% 
  filter(dubbel == 0) %>% 
  select(-dubbel, mac, opzoekdatum = sorteren, titel_in_gids = titel_gids, type = replay, playlist = sched_playlist, duur) %>% 
  filter(playlist != "live > geen playlist nodig")

pl_weekschema <- rbind(pl_weekschema_lgm, pl_weekschema_uzm) %>% 
  select(mac, opzoekdatum, titel_in_gids, type, duur, playlist) %>% 
  arrange(mac)

write_delim(pl_weekschema, delim = "\t", col_names = T, path = "cz_week_20190711.tsv", append = F)
