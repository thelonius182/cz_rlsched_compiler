# init --------------------------------------------------------------------
cfg_cur_week_ymd <- format(date(ymd_hm(config$cur_cz_week)), format = "%Y%m%d")
spb_name <- paste0("spoorboekje_", cfg_cur_week_ymd, ".csv")
plw_output_name <- paste0("Weekschema Playlists, ", cfg_cur_week_ymd, ".pdf")

# stage gidsweek -------------------------------------------------------------
source("src/get_gidsweek.R", encoding = "UTF-8")

valid_spoorboekje <- FALSE

for (seg2 in 1:1) { # creates a break-able segment
  
  #+ controleer of de tabel gevuld is ----
  flog.info("Gidsweek controleren", name = "rlsc_log")
  n_gidsslots <- salsa_plws_gidsweek %>% nrow
  if (n_gidsslots == 0) {
    flog.error("Fout: geen gidsgegevens gevonden voor deze week.", name = "rlsc_log")
    break
  }

  #+ controleer of er uren in de gids ontbreken ----
  gidsgaten <- salsa_plws_gidsweek %>%
    mutate(start_next = lead(pgm_start)) %>%
    filter(!is.na(start_next) & start_next != pgm_stop)
  n_gidsgaten <- gidsgaten %>% nrow
  
  if (n_gidsgaten > 0) {
    plws_notification <- ""
    for (n1 in 1:n_gidsgaten) {
      plws_notification <-
        paste0(plws_notification,
               "\n",
               gidsgaten$pgm_dtm[n1],
               ", ",
               gidsgaten$pgm_stop[n1],
               "u.")
    }
    
    lg_msg <- sprintf("Fout: de gids is niet compleet. Ontbrekende slots: %s", plws_notification)
    flog.error(lg_msg, name = "rlsc_log")
    break
  }
  
  spoorboekje <- salsa_plws_gidsweek %>%
    mutate(cur_cz_week_key = ymd_h(paste0(pgm_dtm, " ", pgm_start)))
  
  # uitzendmac
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
  
  #+ uitzendmac - final order ----
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
                                                      pattern = "in de schijnwerper"))
  
  # plws_e <- pl_weekschema_uzm %>% filter(mac == "U" 
  #                                        & str_detect(string = tolower(titel_in_gids), 
  #                                                     pattern = "leve beethoven"))
  
  plws_e <- pl_weekschema_uzm %>% 
    filter(mac == "U" 
           & !str_detect(string = tolower(type),
                         pattern = "hijack")
           & !str_detect(string = tolower(titel_in_gids), 
                         pattern = "geen dag zonder|de nacht|in de schijnwerper"))
  
  #+ logmac ----
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
  
  
  #+ merge logmac/uitzendmac -------------------------------------------------
  
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
                         plws_e, ws_empty_line 
  ) %>% 
    select(mac, opzoekdatum, titel_in_gids, type, duur, playlist) 
  
  #+ controleer op duplicates ----
  pl_weekschema_dups <-
    pl_weekschema %>% 
    filter(str_length(str_trim(playlist)) > 0) %>% 
    group_by(playlist) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
  
  if (nrow(pl_weekschema_dups) > 0) {
    flog.error("Fout: dubbele files voor zelfde playlist(s) aangetroffen.", name = "rlsc_log")
    flog.info(paste0("Zie playlists ", str_flatten(pl_weekschema_dups$playlist, collapse = ", ")), 
              name = "rlsc_log")
    flog.info("Er is GEEN output gemaakt.", name = "rlsc_log")
    break
  }
  
  #+ OK! ----
  valid_spoorboekje <- TRUE
  flog.info("Gidsweek is in orde", name = "rlsc_log")
  
  #+ vinkvakjes voor Benno ----
  pl_weekschema %<>% mutate(Gereed = if_else(mac %in% c("L", "U"), "O", ""))
  
  #+ render as pdf ----
  # markdown can't see tibbles in 'env', so serialize the weekschema 
  saveRDS(object = pl_weekschema, file = paste0(config$project_home, "cz_rlsched_compiler/plws.RDS"))
  # saveRDS(object = pl_weekschema, file = "g:\\salsa\\plws.RDS\\plws.RDS") # make available to user marimba
  saveRDS(object = pl_weekschema, file = "/cz_salsa/cz_exchange/plws.RDS") # make available to user marimba
  
  flog.info("Genereer de pdf", name = "rlsc_log")
  rmarkdown::render("src/weekschema_playlists.Rmd", output_file = plw_output_name)
  
  #+ move to mac-server ----
  plws_from <- paste0(config$project_home, "cz_rlsched_compiler/src/", plw_output_name)
  plws_to <- "Z:/Shared Items/Kantoor/PROGRAMMAS/Presentatie&Techniek/playlist weekschema's"
  plws_to_delete <- paste0(plws_to, "/", plw_output_name)
  
  if (file_exists(path = plws_to_delete)) {
    file_delete(path = plws_to_delete)
  }
  
  file_copy(path = plws_from, new_path = plws_to)
  flog.info("pdf staat op de mac-server", name = "rlsc_log")
  file_delete(path = plws_from)
  
  # delete .tex-file too
  tex_file <- str_replace(string = plws_from,
                          pattern = "\\.pdf",
                          replacement = ".tex")
  
  if (file_exists(tex_file)) {
    file_delete(path = tex_file)
  }
  
}
