cfg_cur_week_ymd <- format(date(ymd_hm(config$cur_cz_week)), format = "%Y%m%d")
spb_name <- paste0("spoorboekje_", cfg_cur_week_ymd, ".csv")
plw_output_name <- paste0("cz_week_", cfg_cur_week_ymd, ".tsv")

spoorboekje <- read_csv(spb_name) %>%
  mutate(cur_cz_week_key = ymd_h(paste0(pgm_dtm, " ", pgm_start)))

# uitzendmac ---------------------------------------------------------------------

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

# uitzendmac - final order -------------------------------------------------------------

plws_a <- pl_weekschema_uzm %>% filter(mac == "U" 
                                   & str_detect(string = tolower(type), 
                                                pattern = "hijack"))

plws_b <- pl_weekschema_uzm %>% filter(mac == "U" 
                                   & str_detect(string = tolower(titel_in_gids), 
                                                pattern = "geen dag zonder bach"))

plws_c <- pl_weekschema_uzm %>% filter(mac == "U" 
                                   & str_detect(string = tolower(titel_in_gids), 
                                                pattern = "de nacht"))

plws_d <- pl_weekschema_uzm %>% filter(mac == "U" 
                                   & str_detect(string = tolower(titel_in_gids), 
                                                pattern = "componist van de maand"))

plws_e <- pl_weekschema_uzm %>% filter(mac == "U" 
                                   & !str_detect(string = tolower(type),
                                                 pattern = "hijack")
                                   & !str_detect(string = tolower(titel_in_gids), 
                                                 pattern = "geen dag zonder|de nacht|componist van de maand"))

# logmac ---------------------------------------------------------------------

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
  filter(playlist != "live > geen playlist nodig") %>% 
  mutate(type = if_else(titel_in_gids == "Concertzender Actueel" & type == "herhaling", "hijack", type))


# merge logmac/uitzendmac -------------------------------------------------

ws_empty_line <- pl_weekschema_uzm %>% head(n = 1) %>%
  mutate(
    mac = "",
    duur = "",
    titel_in_gids = "",
    herh_van = "",
    hijack = "",
    playlist = "",
    opzoekdatum = "",
    type = ""
  )

pl_weekschema <- rbind(pl_weekschema_lgm, ws_empty_line,
                       plws_a, ws_empty_line,
                       plws_b, ws_empty_line,
                       plws_c, ws_empty_line,
                       plws_d, ws_empty_line, 
                       plws_e
                 ) %>% 
  select(mac, opzoekdatum, titel_in_gids, type, duur, playlist) 

write_delim(pl_weekschema, delim = "\t", col_names = T, path = plw_output_name, append = F)
