build_cur_cz_week <- function(the_cur_week) {

  cz_dt_format <- "%Y-%m-%d %a%H"
  
  the_cur_week %<>%
    mutate(sys_audiotitel = case_when(sched_playlist == "live > geen playlist nodig" ~ 
                                        paste0("Live (", titel, ")"),
                                      is.na(hh_van) ~ 
                                        paste0(format(cz_tijdstip, format = cz_dt_format), 
                                               ".", 
                                               formatC(cz_slot_len, width = 3, format = "d", flag = "0")),
                                      titel_live_jn == "n" ~
                                        paste0(format(hh_van, format = cz_dt_format), 
                                               ".", 
                                               formatC(cz_slot_len, width = 3, format = "d", flag = "0")),
                                      is.na(hh_van_live_jn) ~
                                        "kies een vervangend programma",
                                      hh_van_live_jn == "n" ~
                                        paste0(format(hh_van, format = cz_dt_format), 
                                               ".", 
                                               formatC(cz_slot_len, width = 3, format = "d", flag = "0")),
                                      TRUE ~
                                        paste0("HiJack, ",
                                               format(hh_van, format = cz_dt_format), 
                                               ".", 
                                               formatC(cz_slot_len, width = 3, format = "d", flag = "0")
                                        )
                                      )
    ) %>% 
    select(cz_tijdstip, cz_slot_len, titel_live_jn, sched_playlist, mac, sys_audiotitel)
}
