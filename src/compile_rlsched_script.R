cur_cz_week_lgm <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% 
  filter(uitzend_mac_jn == "n") %>% 
  filter(!(titel == "Thema" & hour(cz_tijdstip) == 21)) %>% 
  select(-starts_with("week"), -uitzend_mac_jn) %>% 
  mutate(pl_titel = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>% 
  arrange(cz_tijdstip)

cur_cz_week_uzm <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% 
  filter(uitzend_mac_jn == "j") %>% 
  filter(!(titel == "Thema" & hour(cz_tijdstip) == 21)) %>% 
  select(-starts_with("week"), -uitzend_mac_jn) %>% 
  mutate(pl_titel = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>% 
  arrange(cz_tijdstip)

build_rl_script <- function(playlist) {
  # playlist <- "20181118_zo10.060_een_vroege_wandeling"
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Info types samenstellen - zie tabblad "schedule_radiologik"
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # const_0
  sch01_C0 <- "Radiologik Schedule Segment" %>% as_tibble
  
  # dag
  sch01_dag <- rls_dagletters(playlist) %>% as_tibble  
  
  # lengte
  sch01_lengte <- rls_lengte(playlist) %>% as_tibble
  
  # dj_voorkeur
  sch01_dj_voorkeur <- "Default" %>% as_tibble
  
  # stuur_naar_dj
  sch01_stuur_naar_dj <- "ProgramTo=0" %>% as_tibble
  
  # start
  sch01_start <- rls_30m_blokken(playlist) %>% as_tibble
  
  # const_4
  sch01_C4 <- "0" %>% as_tibble
  
  # leeg_1
  sch01_leeg_1 <- "" %>% as_tibble
  
  # venster_van en -tot
  v_limiet <- rls_venster(playlist)
  sch01_venster_van <- paste(v_limiet[1], "0", sep = "\t") %>% as_tibble 
  sch01_venster_tot <- paste(v_limiet[2], "0", sep = "\t") %>% as_tibble
  
  # const_8
  sch01_C8 <- "ProgramCopyPath=nopath" %>% as_tibble
  
  # color
  sch01_color <- "ColorLabel=238,238,153" %>% as_tibble # oranje 255,128,0 | geel 255,204,102
  
  # const_10
  sch01_const_10 <- "0" %>% as_tibble
  
  # leeg_2
  sch01_leeg_2 <- "" %>% as_tibble
  
  # const_12
  sch01_const_12 <- "Display=True" %>% as_tibble
  
  # const_13
  sch01_const_13 <- "PlayRotatediniTunes=False" %>% as_tibble
  
  # const_14
  sch01_const_14 <- "Notes=" %>% as_tibble
  
  # const_15
  sch01_const_15 <- paste("PrePostAppleScripts=", "", sep = "\t") %>% as_tibble
  
  # const_16
  sch01_const_16 <- "AlbumSeparation=0" %>% as_tibble
  
  # const_17
  sch01_const_17 <- "Begin Script" %>% as_tibble
  
  # play
  sch01_play <- paste("play", "00:00", "",          "",          "",
                      pl_name, "", "", "", "", "", "", "", sep = "\t") %>% as_tibble
  
  script_file <- bind_rows(sch01_C0,
                           sch01_dag,
                           sch01_lengte,
                           sch01_dj_voorkeur, 
                           sch01_stuur_naar_dj,
                           sch01_start, 
                           sch01_C4, 
                           sch01_leeg_1,
                           sch01_venster_van,
                           sch01_venster_tot, 
                           sch01_C8,
                           sch01_color,
                           sch01_const_10,
                           sch01_leeg_2,
                           sch01_const_12,
                           sch01_const_13,
                           sch01_const_14,
                           sch01_const_15,
                           sch01_const_16,
                           sch01_const_17,
                           sch01_play
  )
  
  # zet de startscripts voor de playlists in de schedules-map van RL, naam begint met
  # een volgnummer: 1 + <aantal scripts in deze map>
  home_radiologik_schedules <- paste0(home_prop("home_radiologik"), "Schedule/")
  nrow_schedules <- 1L + dir_ls(path = home_radiologik_schedules) %>% as_tibble %>% nrow
  script_file_name <- sprintf(paste0(home_radiologik_schedules, "%03d - ", playlist), nrow_schedules)
  write.table(x = script_file, file = script_file_name, row.names = FALSE, col.names = FALSE, 
              sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
}

rls_dagletters <- function(some_playlist) {
  # some_playlist <- "20180603_wo07.060_de_titel_klassiek"
  dag_kort <- str_sub(some_playlist, 10, 11)
  rls_dagletters_result <- case_when(dag_kort == "ma" ~ "_M_____",
                                     dag_kort == "di" ~ "__T____",
                                     dag_kort == "wo" ~ "___W___",
                                     dag_kort == "do" ~ "____T__",
                                     dag_kort == "vr" ~ "_____F_",
                                     dag_kort == "za" ~ "______S",
                                     TRUE             ~ "S______"
  )
}

rls_lengte <- function(some_playlist) {
  # some_playlist <- "20180603_wo07.060_de_titel_klassiek"
  rls_lengte_result <- str_sub(some_playlist, 15, 17) %>% as.integer %>% as.character
}

rls_30m_blokken <- function(some_playlist){
  # some_playlist <- "20180603_wo07.180_de_titel_klassiek"
  rls_30m_blokken_result <- some_playlist %>% str_sub(12, 13) %>% as.integer
  rls_30m_blokken_result <- as.character(2 * rls_30m_blokken_result)
}

rls_venster <- function(some_playlist) {
  # some_playlist <- "20181231_wo00.420_de_nacht_klassiek"
  venster_datum_start <- str_sub(some_playlist, 1, 8) %>% ymd
  venster_datum_stop <- venster_datum_start + days(1) # hier zat de herhaling: days(8)
  rl_date_fmt <- stamp_date("23 mrt. 2018")
  venster_datum_start %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  venster_datum_stop %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  rls_venster_result <- c(venster_datum_start, venster_datum_stop)
}