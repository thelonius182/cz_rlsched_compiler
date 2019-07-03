cur_cz_week_uzm_tsv <- cur_cz_week_uzm %>% 
  mutate(cz_van = format(cz_tijdstip, format = "%Y%m%d_%H"),
         cz_tot = format(cz_tijdstip + minutes(cz_slot_len), format = "%Y%m%d_%H"),
         mac = "U") %>% 
  select(cz_van, cz_tot, mac, sched_playlist) 

cur_cz_week_lgm_tsv <- cur_cz_week_lgm %>% 
  mutate(cz_van = format(cz_tijdstip, format = "%Y%m%d_%H"),
         cz_tot = format(cz_tijdstip + minutes(cz_slot_len), format = "%Y%m%d_%H"),
         mac = "L") %>% 
  select(cz_van, cz_tot, mac, sched_playlist)  

cur_cz_week_tsv <- rbind(cur_cz_week_uzm_tsv, cur_cz_week_lgm_tsv) %>% 
  arrange(cz_van)

write_delim(cur_cz_week_tsv, delim = "\t", col_names = T, path = "cz_week.tsv", append = F)

r4_corr_hijack <- montage %>% 
  mutate(hh_van = format(cz_tijdstip, "%Y%m%d_%H")) %>% 
  select(-cz_tijdstip)

r4_corr <- gs_title("R4-correcties")

r4_corr_stage <- r4_corr %>% 
  gs_read(ws = "corr_full") %>% 
  select(`hh-van`) %>%
  rename(hh_van = `hh-van`) %>% 
  filter(!hh_van == "-") %>% 
  left_join(r4_corr_hijack) %>% 
  filter(mtr_live_jn == "j")

saveRDS(spoorboekje_20190704, file = "spoorboekje.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# versie 27 juni
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
pl_weekschema_lgm <- spoorboekje %>% 
  mutate(cur_cz_week_uzm_key = ymd_h(paste0(pgm_dtm, " ", pgm_start))) %>% 
  left_join(cur_cz_week_uzm, by = c("cur_cz_week_uzm_key" = "cz_tijdstip")) %>% 
  filter(!is.na(sched_playlist)) %>% 
  select(mac, pgm_dtm, pgm_start, cz_slot_len, titel_gids = title, 
         herh_van, sched_playlist, sys_audiotitel, cur_cz_week_uzm_key) %>% 
  mutate(bijz = str_replace_all(string = sys_audiotitel, 
                                pattern = "\\d{4}-\\d{2}-\\d{2} \\w{4}\\.\\d{3}|,|Live \\(.*", 
                                replacement = ""),
         uitzending = paste0(format(cur_cz_week_uzm_key, format = "%Y-%m-%d %a%H"),
                                     ".",
                                     str_pad(cz_slot_len, width = 3, side = "left", pad = "0")
                             ),
         sorteren = if_else(herh_van == "-", format(cur_cz_week_uzm_key, format = "%Y%m%d_%H"), herh_van)
  ) %>%
  select(mac, uitzending, titel_gids, herh_van, hijack = bijz, sched_playlist, sorteren) %>% 
  arrange(desc(hijack), mac, sorteren) %>% 
  mutate(dubbel = if_else(lag(sorteren) == sorteren, 1, 0),
         replay = if_else(trimws(hijack) == "HiJack", hijack, if_else(herh_van == "-", "nieuw", "herhaling"))
  ) %>% 
  filter(dubbel == 0) %>% 
  select(-dubbel) %>% 
  rename(opzoekdatum = sorteren) %>% 
  select(mac, opzoekdatum, titel_in_gids = titel_gids, type = replay, playlist = sched_playlist, uitzending) %>% 
  filter(playlist != "live > geen playlist nodig")

pl_weekschema_lgm <- spoorboekje %>% 
  mutate(cur_cz_week_lgm_key = ymd_h(paste0(pgm_dtm, " ", pgm_start))) %>% 
  left_join(cur_cz_week_lgm, by = c("cur_cz_week_lgm_key" = "cz_tijdstip")) %>% 
  filter(!is.na(sched_playlist)) %>% 
  select(mac, pgm_dtm, pgm_start, cz_slot_len, titel_gids = title, 
         herh_van, sched_playlist, sys_audiotitel, cur_cz_week_lgm_key) %>% 
  mutate(bijz = str_replace_all(string = sys_audiotitel, 
                                pattern = "\\d{4}-\\d{2}-\\d{2} \\w{4}\\.\\d{3}|,|Live \\(.*", 
                                replacement = ""),
         uitzending = paste0(format(cur_cz_week_lgm_key, format = "%Y-%m-%d %a%H"),
                             ".",
                             str_pad(cz_slot_len, width = 3, side = "left", pad = "0")
         ),
         sorteren = if_else(herh_van == "-", format(cur_cz_week_lgm_key, format = "%Y%m%d_%H"), herh_van)
  ) %>%
  select(mac, uitzending, titel_gids, herh_van, hijack = bijz, sched_playlist, sorteren) %>% 
  arrange(desc(hijack), mac, sorteren) %>% 
  mutate(dubbel = if_else(lag(sorteren) == sorteren, 1, 0),
         replay = if_else(trimws(hijack) == "HiJack", hijack, if_else(herh_van == "-", "nieuw", "herhaling"))
  ) %>% 
  filter(dubbel == 0) %>% 
  select(-dubbel) %>% 
  rename(opzoekdatum = sorteren) %>% 
  select(mac, opzoekdatum, titel_in_gids = titel_gids, type = replay, playlist = sched_playlist, uitzending) %>% 
  filter(playlist != "live > geen playlist nodig")

pl_weekschema <- rbind(pl_weekschema_lgm, pl_weekschema_lgm) %>% 
  arrange(mac)

write_delim(pl_weekschema, delim = "\t", col_names = T, path = "cz_week_20190627.tsv", append = F)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# versie 4 juli
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
spoorboekje <- spoorboekje_20190704 %>% 
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

write_delim(pl_weekschema, delim = "\t", col_names = T, path = "cz_week_20190704.tsv", append = F)
