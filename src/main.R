# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Genereer RL-schedules voor wekelijke CZ-zenderformat
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Init
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(tidyr)
library(knitr)
library(rmarkdown)
library(googlesheets)
library(RCurl)
library(yaml)
library(magrittr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(fs)
library(readr)
library(futile.logger)
library(keyring)
library(RMySQL)
library(officer)

filter <- dplyr::filter # voorkom verwarring met stats::filter

home_prop <- function(prop) {
  prop_name <- paste0(prop, ".", host)
  prop <- config[[prop_name]] %>% 
    curlEscape %>% 
    str_replace_all(pattern = "\\%2F", replacement = "/")
}

flog.appender(appender.file("/Users/nipper/Logs/rlsched_compiler.log"), name = "rlsc_log")
flog.info("= = = = = RL-schedulerscript Compiler start = = = = =", name = "rlsc_log")

config <- read_yaml("config.yaml")
source(config$toolbox, encoding = "UTF-8")

# Modelrooster openen ----
modelrooster <- readRDS(paste0(config$giva.rds.dir, "zenderschema.RDS"))

moro_clean1 <- modelrooster %>%
  mutate(dag = str_sub(slot, 1, 2),
         start = paste(str_sub(slot, 3, 4), str_sub(slot, 5, 7), sep = ".")
  ) %>% 
  select(-balk,
         -balk_tonen,
         -starts_with("bijzonderheden"),
         -starts_with("product"),
         -slot,
         hh_offset = hh_formule
  )

moro_long <-
  gather(
    data = moro_clean1,
    wekelijks,
    week_1,
    week_2,
    week_3,
    week_4,
    week_5,
    AB_cyclus,
    key = "weken",
    value = "titel"
  ) %>% 
  filter(!is.na(titel)) %>% 
  mutate(
    cz_moro_slot = paste0(str_sub(dag, 1, 2), str_sub(start, 1, 2)),
    cz_moro_slot_len = as.integer(str_sub(start, 4, 6)),
    titel = sub("(.*)(?: S\\d)$", "\\1", titel, perl = TRUE),
    thema_detect = paste(dag, start, titel, sep = "¶")
  ) %>%
  # corrigeer "Thema": niet 60 maar 120 minuten, en niet 2 slots maar 1!
  filter(thema_detect != "wo¶21.060¶Thema") %>% 
  mutate(start = if_else(thema_detect == "wo¶20.060¶Thema", "20.120", start),
         cz_moro_slot_len = if_else(thema_detect == "wo¶20.060¶Thema", 120L, cz_moro_slot_len)
  ) %>% 
  arrange(cz_moro_slot, weken)

# run_cz_week is het begintijdstip van de week waarvoor in deze run scripts gemaakt moeten worden
# cz_week_start is het begintijdstip van de kwartaalreeks die voor het maken vd scripts nodig is
#   zodat elke titel minstens 1 x in de reeks zit; daar zijn 13 weken voor nodig.
run_cz_week <- config$cur_cz_week
cz_week_start <- ymd_hm(run_cz_week) - days(14)

cz_cal <- seq(cz_week_start, length.out = 13 * 168, by = "hours") %>% 
  tibble::enframe(name = NULL)

names(cz_cal) <- "cz_cal_datetime"

cz_cal %<>% mutate(
  cz_cal_slot = format(cz_cal_datetime, format = "%a%H"),
  moro_week = case_when(
    day(cz_cal_datetime) > 28 ~ "week_5|wekelijks|AB_cyclus",
    day(cz_cal_datetime) > 21 ~ "week_4|wekelijks|AB_cyclus",
    day(cz_cal_datetime) > 14 ~ "week_3|wekelijks|AB_cyclus",
    day(cz_cal_datetime) >  7 ~ "week_2|wekelijks|AB_cyclus",
    TRUE                      ~ "week_1|wekelijks|AB_cyclus"
  )
)

cz_cal_moro <- cz_cal %>% 
  inner_join(moro_long, by = c("cz_cal_slot" = "cz_moro_slot")) %>%
  filter(str_detect(string = weken, pattern = moro_week))

cz_cal_moro_hh <- cz_cal_moro %>%
  mutate(
    hh_1 = str_sub(hh_offset, 1, 2),
    # hh_inc_dagen = if_else(hh_1 == "tw", 7L, as.integer(hh_1)),
    hh_inc_dagen = as.integer(hh_1),
    cz_datetime_hh = cz_cal_datetime + days(hh_inc_dagen),
    # uu_1 = if_else(hh_offset == "tw", str_sub(cz_slot, 3, 4), str_sub(hh_offset, 5, 6)),
    uu_1 = str_sub(hh_offset, 5, 6)
  ) %>% 
  rename(cz_slot = cz_cal_slot, cz_slot_len = cz_moro_slot_len)

hour(cz_cal_moro_hh$cz_datetime_hh) <- as.integer(cz_cal_moro_hh$uu_1)

# weken samenstellen ------------------------------------------------------

week_orig <- cz_cal_moro_hh %>%
  rename(cz_tijdstip = cz_cal_datetime) %>%
  select(cz_tijdstip, cz_slot, cz_slot_len, titel) %>%
  mutate(hh_van = NA, hh_van_slot = NA)

week_herh <- cz_cal_moro_hh %>%
  rename(cz_tijdstip = cz_datetime_hh, hh_van = cz_cal_datetime) %>%
  select(cz_tijdstip, cz_slot_len, titel, hh_van, hh_van_slot = cz_slot) %>%
  mutate(cz_slot = format(cz_tijdstip, format = "%a%H")) %>% 
  select(cz_tijdstip, cz_slot, cz_slot_len, titel, hh_van, hh_van_slot) %>%
  filter(!is.na(cz_tijdstip))

cz_week <- bind_rows(week_orig, week_herh) %>%
  mutate(weekschema = cz_week_start + days(7L * (as.duration(cz_week_start %--% cz_tijdstip) / dhours(1)) %/% 168L),
         weekschema_hh = cz_week_start + days(7L * (as.duration(cz_week_start %--% hh_van) / dhours(1)) %/% 168L)
         ) %>%
  arrange(titel, weekschema, cz_tijdstip)

# zelfde titel > 1 keer in zelfde weekschema
cz_week_grp1 <- cz_week %>% group_by(titel, weekschema) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  select(titel) %>% 
  unique

# zelfde titel in zelfde weekschema uit verschillende periodes
# tezamen levert dat de titels die een aparte replay-playlist nodig hebben
cz_titels_met_replay_playlist <- cz_week %>% 
  inner_join(cz_week_grp1) %>% 
  mutate(playlist = titel, playlist_hh = paste0(titel, " (herhaling)")) %>% 
  filter(!is.na(weekschema_hh) & weekschema != weekschema_hh) %>% 
  select(titel, playlist, playlist_hh) %>% unique

cz_week_grp2 <- cz_cal_moro_hh %>% select(titel) %>% unique %>% 
  filter(!str_detect(tolower(titel), pattern = "ochtend")) %>% 
  arrange(titel) %>% 
  left_join(cz_titels_met_replay_playlist) %>% 
  select(-playlist)

# apple_scripts - prep voor aanmaken iTunes playlists ---------------------

itunes_cupboard_stage <- readRDS(paste0(config$giva.rds.dir, "itunes_cupboard.RDS"))

itunes_cupboard <- itunes_cupboard_stage %>% 
  inner_join(cz_week_grp2) %>% # join by titel
  arrange(uitzendmac_jn, titel_live_jn, titel)

# uitzendmac

ipl_uzm_live_stage <- itunes_cupboard %>% rename(playlist = titel) %>% 
  filter(uitzendmac_jn == "j" & titel_live_jn == "j") %>% 
  select(playlist) %>% arrange(playlist)

ipl_uzm_live_hh_stage <- itunes_cupboard %>% rename(playlist = playlist_hh) %>% 
  filter(uitzendmac_jn == "j" & titel_live_jn == "j" & !is.na(playlist)) %>% 
  select(playlist) %>% arrange(playlist)

ipl_uzm_semilive_stage <- itunes_cupboard %>% rename(playlist = titel) %>%  
  filter(uitzendmac_jn == "j" & titel_live_jn == "n") %>% 
  select(playlist) %>% arrange(playlist)

ipl_uzm_semilive_hh_stage <- itunes_cupboard %>% rename(playlist = playlist_hh) %>% 
  filter(uitzendmac_jn == "j" & titel_live_jn == "n" & !is.na(playlist)) %>% 
  select(playlist) %>% arrange(playlist)

# logmac

ipl_lgm_live_stage <- itunes_cupboard %>% rename(playlist = titel) %>% 
  filter(uitzendmac_jn == "n" & titel_live_jn == "j") %>% 
  select(playlist) %>% arrange(playlist)

ipl_lgm_live_hh_stage <- itunes_cupboard %>% rename(playlist = playlist_hh) %>%  
  filter(uitzendmac_jn == "n" & titel_live_jn == "j" & !is.na(playlist)) %>% 
  select(playlist) %>% arrange(playlist)

ipl_lgm_semilive_stage <- itunes_cupboard %>% rename(playlist = titel) %>%  
  filter(uitzendmac_jn == "n" & titel_live_jn == "n") %>% 
  select(playlist) %>% arrange(playlist)

ipl_lgm_semilive_hh_stage <- itunes_cupboard %>% rename(playlist = playlist_hh) %>%  
  filter(uitzendmac_jn == "n" & titel_live_jn == "n" & !is.na(playlist)) %>% 
  select(playlist) %>% arrange(playlist)

# bind rows uitzend + logmac

ipl_uzm_live <- bind_rows(ipl_uzm_live_stage, ipl_uzm_live_hh_stage) %>% 
  arrange(playlist)

ipl_uzm_semilive <- bind_rows(ipl_uzm_semilive_stage, ipl_uzm_semilive_hh_stage) %>% 
  arrange(playlist)

ipl_lgm_live <- bind_rows(ipl_lgm_live_stage, ipl_lgm_live_hh_stage) %>% 
  arrange(playlist)

ipl_lgm_semilive <- bind_rows(ipl_lgm_semilive_stage, ipl_lgm_semilive_hh_stage) %>% 
  arrange(playlist)

rm(
  ipl_lgm_live_hh_stage,
  ipl_lgm_live_stage,
  ipl_lgm_semilive_hh_stage,
  ipl_lgm_semilive_stage,
  ipl_uzm_live_hh_stage,
  ipl_uzm_live_stage,
  ipl_uzm_semilive_hh_stage,
  ipl_uzm_semilive_stage
)

# apple_scripts - genereer --------------------------------------------------

# source("src/build_applescript_to_create_ipls.R", encoding = "UTF-8")
# LET OP! elke build genereert telkens dezeldfe file: ascr.txt. 
# per file dus steeds met de hand: copy/paste in mac-scripteditor & run
# LET OP #2!! de scripts voor de semi-live folder op iTunes krijgen /parent-of-playlist = "live_ref"/ 
#             dat met de hand ff wijzigen in "semi_live_ref"

# build_applescript_ipls(ipl_lgm_live)
# build_applescript_ipls(ipl_lgm_semilive)
# 
# build_applescript_ipls(ipl_uzm_live)
# build_applescript_ipls(ipl_uzm_semilive)


# montagerooster koppelen -----------------------------------------------------
cur_cz_week_lgm_prep <- cz_week %>% 
  filter(weekschema == ymd_hm(run_cz_week)) %>% 
  inner_join(itunes_cupboard) %>% # Joining, by = "titel"
  filter(uitzendmac_jn == "n") %>% 
  select(-starts_with("week"), -uitzendmac_jn) %>% 
  mutate(sched_playlist = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>%
  arrange(titel)

cur_cz_week_uzm_prep <- cz_week %>% 
  mutate(hczt = hour(cz_tijdstip), hhhv = hour(hh_van)) %>% 
  filter(weekschema == ymd_hm(run_cz_week)) %>% 
  inner_join(itunes_cupboard) %>% # Joining, by = "titel"
  filter(uitzendmac_jn == "j") %>% 
  select(-starts_with("week"), -uitzendmac_jn) %>% 
  mutate(sched_playlist = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>%
  arrange(titel)

montage.I <- readRDS(paste0(config$giva.rds.dir, "montage.RDS")) 

montage <- montage.I %>% 
  mutate(cz_tijdstip = ymd_h(paste0(uitzending, " ", str_sub(slot, 3, 4))),
         mtr_live_jn = if_else(str_to_lower(product) == "live", "j", "n")) %>% 
  select(cz_tijdstip, mtr_live_jn) %>% 
  filter(!is.na(cz_tijdstip))

# huidige week voorbereiden ----

cur_cz_week_uzm <- cur_cz_week_uzm_prep %>% 
  left_join(montage, by = c("hh_van" = "cz_tijdstip")) %>% 
  rename(hh_van_live_jn = mtr_live_jn) %>% 
  left_join(montage) %>%  # Joining, by = "cz_tijdstip"
  rename(nieuw_live_jn = mtr_live_jn) %>%
  mutate(mac = "U")
  
cur_cz_week_lgm <- cur_cz_week_lgm_prep %>% 
  left_join(montage, by = c("hh_van" = "cz_tijdstip")) %>% 
  rename(hh_van_live_jn = mtr_live_jn) %>% 
  left_join(montage) %>%  # Joining, by = "cz_tijdstip"
  rename(nieuw_live_jn = mtr_live_jn) %>%
  mutate(mac = "L")

# live-programma's hebben geen script of playlist nodig

cur_cz_week_uzm %<>% 
  mutate(sched_playlist = if_else(nieuw_live_jn == "n" | is.na(nieuw_live_jn), 
                                  sched_playlist, 
                                  "live > geen playlist nodig"))

cur_cz_week_lgm %<>% 
  mutate(sched_playlist = if_else(nieuw_live_jn == "n" | is.na(nieuw_live_jn), 
                                  sched_playlist, 
                                  "live > geen playlist nodig"))

# semi-live programma's: systeemdeel van audiofiles (jaar-mnd-dag-dagnaam-uur-duur)
source("src/compile_cur_week.R", encoding = "UTF-8")
cur_cz_week_uzm <- build_cur_cz_week(cur_cz_week_uzm)
# saveRDS(object = cur_cz_week_uzm, file = "g:\\salsa\\cur_cz_week_uzm.RDS") # available to marimba user
saveRDS(object = cur_cz_week_uzm, file = "/cz_salsa/cz_exchange/cur_cz_week_uzm.RDS") # available to marimba user
cur_cz_week_lgm <- build_cur_cz_week(cur_cz_week_lgm)
source("src/compile_pl_wk_schema.R", encoding = "UTF-8")

# Vervang de scripts ----
if (valid_spoorboekje) {

  source("src/ditch_expired_sched_scripts.R", encoding = "UTF-8")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #+ op Uitzend-mac ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  flog.info("Start scriptgeneratie op de Uitzend-mac", name = "rlsc_log")
  host <- "uitzendmac"
  home_radiologik <- home_prop("home_radiologik")
  switch_home <- paste0(home_prop("home_schedulerswitch"), "/nipper_msg.txt")
  
  # Stop RL-scheduler en wacht 5 seconden - stoppen duurt soms even
  flog.info("RL-scheduler stoppen", name = "rlsc_log")
  switch <- read_lines(file = switch_home)
  switch <- "stop RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  
  Sys.sleep(time = 5)
  flog.info("RL-scheduler is gestopt", name = "rlsc_log")
  
  #+... gooi verlopen scripts weg ----
  uzm_path <- "//UITZENDMAC-CZ/Radiologik/Schedule"
  
  # Ochtendeditie-scripts
  oe_sched <- dir_info(path = uzm_path) %>% 
    filter(str_detect(path, "ochtendeditie")) %>% 
    mutate(script_item = str_sub(path, 37, 39),
           script_date = str_sub(path, 43, 50),
           script_hour = str_sub(path, 54, 55),
           script_length = str_sub(path, 57, 59)
    ) %>% 
    select(path, starts_with("script"))
  
  oe_expiration <- oe_sched %>% 
    mutate(script_ymdh_s = paste0(script_date, " ", script_hour, ":00"),
           script_ymdh = ymd_hm(script_ymdh_s, tz = "Europe/Amsterdam"),
           # script verloopt 1 uur na einde uitzending; voor OE komen daar 7 dagen bij, vanwege herhaling
           script_expires = script_ymdh + minutes(as.integer(script_length) + 60) + days(7),
           script_interval = interval(script_expires, now(), tz = "Europe/Amsterdam"),
           expired_h = round(script_interval / dhours(1), digits = 1)
    )
  
  oe_files_to_ditch <- oe_expiration %>% 
    select(-starts_with("script")) %>% 
    filter(expired_h > 0.0) %>% 
    select(path)
  
  for (a_file in oe_files_to_ditch$path) {
    file_delete(a_file)
  }
  
  # weekscripts
  uzm_weekfiles_to_ditch <- get_weekfiles_to_ditch(uzm_path)
  
  for (a_file in uzm_weekfiles_to_ditch$dir_info_path) {
    file_delete(a_file)
  }
  
  #+... herstel volgorde ----
  resequence_script_names(uzm_path)
  
  #+... genereer nieuwe ----
  scheds_prep <- cur_cz_week_uzm %>% 
    filter(sched_playlist != "live > geen playlist nodig") %>% 
    mutate(ts_playlist = paste0(format(cz_tijdstip, format = "%Y-%m-%d %a%H"),
                                ".",
                                str_pad(cz_slot_len, width = 3, side = "left", pad = "0"),
                                " ",
                                sched_playlist)
    ) %>% 
    select(ts_playlist) %>% 
    arrange(ts_playlist)
  
  source("src/compile_rlsched_script.R", encoding = "UTF-8")
  cur_kleurschema <- "geel"
  
  for (cur_ts_playlist in scheds_prep$ts_playlist) {
    cur_kleurschema <- if_else(cur_kleurschema == "oranje", "geel", "oranje")
    build_rl_script(cur_ts_playlist, cur_kleurschema)  
    flog.info("Script toegevoegd: %s", cur_ts_playlist, name = "rlsc_log")
  }
  
  # RL-scheduler herstarten
  flog.info("Compiler gereed, start RL-scheduler", name = "rlsc_log")
  switch <- read_lines(file = switch_home)
  switch <- "start RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  flog.info("RL-scheduler draait weer", name = "rlsc_log")
  flog.info("- - - - - - - - - -", name = "rlsc_log")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # + op Log-mac ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  flog.info("Start scriptgeneratie op de Log-mac", name = "rlsc_log")
  host <- "logmac"
  home_radiologik <- home_prop("home_radiologik")
  switch_home <- paste0(home_prop("home_schedulerswitch"), "/nipper_msg.txt")
  
  # Stop RL-scheduler en wacht 5 seconden - stoppen duurt soms even
  flog.info("RL-scheduler stoppen", name = "rlsc_log")
  switch <- read_lines(file = switch_home)
  switch <- "stop RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  
  Sys.sleep(time = 5)
  flog.info("RL-scheduler is gestopt", name = "rlsc_log")
  
  #+... gooi verlopen scripts weg ----
  lgm_path <- "//LOGMAC/Radiologik/Schedule"
  
  lgm_weekfiles_to_ditch <- get_weekfiles_to_ditch(lgm_path)
  
  for (a_file in lgm_weekfiles_to_ditch$dir_info_path) {
    file_delete(a_file)
  }
  
  
  #+... herstel volgorde ----
  resequence_script_names(lgm_path)
  
  #+... genereer nieuwe ----
  scheds_prep <- cur_cz_week_lgm %>% 
    filter(sched_playlist != "live > geen playlist nodig") %>% 
    mutate(ts_playlist = paste0(format(cz_tijdstip, format = "%Y-%m-%d %a%H"),
                                ".",
                                str_pad(cz_slot_len, width = 3, side = "left", pad = "0"),
                                " ",
                                sched_playlist)
    ) %>% 
    select(ts_playlist) %>% 
    arrange(ts_playlist)
  
  source("src/compile_rlsched_script.R", encoding = "UTF-8")
  cur_kleurschema <- "geel"
  
  for (cur_ts_playlist in scheds_prep$ts_playlist) {
    cur_kleurschema <- if_else(cur_kleurschema == "oranje", "geel", "oranje")
    build_rl_script(cur_ts_playlist, cur_kleurschema)  
    flog.info("Script toegevoegd: %s", cur_ts_playlist, name = "rlsc_log")
  }
  
  # RL-scheduler herstarten
  flog.info("Compiler gereed, start RL-scheduler", name = "rlsc_log")
  switch <- read_lines(file = switch_home)
  switch <- "start RL-scheduler"
  write_lines(switch, file = switch_home, append = FALSE)
  flog.info("RL-scheduler draait weer", name = "rlsc_log")
  
  flog.info("= = = = = RL-schedulerscript Compiler stop = = = = =", name = "rlsc_log")

} # exclude curly br. when running manually!
