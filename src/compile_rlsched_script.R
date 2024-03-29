build_rl_script <- function(playlist, kleurschema) {
  # playlist <- "2018-11-18 zo10.060 Bijdetijds (herhaling)"
  # kleurschema <- "oranje"
  pl_name <- str_sub(playlist, 21)
  playlist <- str_replace_all(playlist, pattern = "\\.| ", replacement = "_")
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Info types samenstellen - zie tabblad "schedule_radiologik"
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # const_0
  sch01_C0 <- "Radiologik Schedule Segment" %>% tibble::enframe(name = NULL)
  
  # dag
  sch01_dag <- rls_dagletters(playlist) %>% tibble::enframe(name = NULL)  
  
  # lengte
  sch01_lengte <- rls_lengte(playlist) %>% tibble::enframe(name = NULL)
  
  # dj_voorkeur
  sch01_dj_voorkeur <- "standaard" %>% tibble::enframe(name = NULL)
  
  # stuur_naar_dj
  sch01_stuur_naar_dj <- "ProgramTo=0" %>% tibble::enframe(name = NULL)
  
  # start
  sch01_start <- rls_30m_blokken(playlist) %>% tibble::enframe(name = NULL)
  
  # const_4
  sch01_C4 <- "0" %>% tibble::enframe(name = NULL)
  
  # leeg_1
  sch01_leeg_1 <- "" %>% tibble::enframe(name = NULL)
  
  # venster_van en -tot
  v_limiet <- rls_venster(playlist)
  sch01_venster_van <- paste(v_limiet[1], "0", sep = "\t") %>% tibble::enframe(name = NULL) 
  sch01_venster_tot <- paste(v_limiet[2], "0", sep = "\t") %>% tibble::enframe(name = NULL)
  
  # const_8
  sch01_C8 <- "ProgramCopyPath=nopath" %>% tibble::enframe(name = NULL)
  
  # color
  sch01_color_prep <- if_else(kleurschema == "geel", "255,204,102", "255,128,0")
  sch01_color <- paste0("ColorLabel=", sch01_color_prep) %>% tibble::enframe(name = NULL) # oranje 255,128,0 | geel 255,204,102
  
  # const_10
  sch01_const_10 <- "0" %>% tibble::enframe(name = NULL)
  
  # leeg_2
  sch01_leeg_2 <- "" %>% tibble::enframe(name = NULL)
  
  # const_12
  sch01_const_12 <- "Display=True" %>% tibble::enframe(name = NULL)
  
  # const_13
  sch01_const_13 <- "PlayRotatediniTunes=False" %>% tibble::enframe(name = NULL)
  
  # const_14
  sch01_const_14 <- "Notes=" %>% tibble::enframe(name = NULL)
  
  # const_15
  sch01_const_15 <- paste("PrePostAppleScripts=", "", sep = "\t") %>% tibble::enframe(name = NULL)
  
  # const_16
  sch01_const_16 <- "AlbumSeparation=0" %>% tibble::enframe(name = NULL)
  
  # const_17
  sch01_const_17 <- "Begin Script" %>% tibble::enframe(name = NULL)
  
  # play
  sch01_play <- paste("play", "00:00", "",          "",          "",
                      pl_name, "", "", "", "", "", "", "", sep = "\t") %>% tibble::enframe(name = NULL)
  
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
  nrow_schedules <- 1L + dir_ls(path = home_radiologik_schedules) %>% 
    tibble::enframe(name = NULL) %>% 
    nrow
  script_file_name <- sprintf(paste0(home_radiologik_schedules, "%03d - ", playlist), nrow_schedules)
  write.table(x = script_file, file = script_file_name, row.names = FALSE, col.names = FALSE, 
              sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
}

rls_dagletters <- function(some_playlist) {
  # some_playlist <- "2018-06-03_wo07.060_de_titel_klassiek"
  dag_kort <- str_sub(some_playlist, 12, 13)
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
  # some_playlist <- "2018-06-03_wo07.060_de_titel_klassiek"
  rls_lengte_result <- str_sub(some_playlist, 17, 19) %>% as.integer %>% as.character
}

rls_30m_blokken <- function(some_playlist){
  # some_playlist <- "2018-06-03_wo07.180_de_titel_klassiek"
  rls_30m_blokken_result <- some_playlist %>% str_sub(14, 15) %>% as.integer
  rls_30m_blokken_result <- as.character(2 * rls_30m_blokken_result)
}

rls_venster <- function(some_playlist) {
  # some_playlist <- "2018-12-31 wo00.420 de nacht klassiek"
  venster_datum_start <- str_sub(some_playlist, 1, 10) %>% ymd
  venster_datum_stop <- venster_datum_start + ddays(1) # hier zat de herhaling: days(8)
  
  # UZM
  # rl_date_fmt <- stamp_date("23 mrt. 2018")
  # venster_datum_start %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  # venster_datum_stop %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  
  # UZM-2
  rl_date_fmt <- stamp_date("23 May 2018")
  venster_datum_start %<>% rl_date_fmt 
  venster_datum_stop %<>% rl_date_fmt 
  
  rls_venster_result <- c(venster_datum_start, venster_datum_stop)
}
