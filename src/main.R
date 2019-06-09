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

host <- config$host
home_radiologik <- home_prop("home_radiologik")
# switch_home <- paste0(home_prop("home_schedulerswitch"), "/nipper_msg.txt")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Stop RL-scheduler op de mac en wacht 5 seconden - stoppen duurt soms even
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# flog.info("RL-scheduler stoppen", name = "nipperlog")
# switch <- read_lines(file = switch_home)
# switch <- "stop RL-scheduler"
# write_lines(switch, path = switch_home, append = FALSE)

# Sys.sleep(time = 5)
# flog.info("RL-scheduler is gestopt", name = "nipperlog")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Modelrooster op GD openen
# NB!! zonodig: change to new user; er opent een browser dialogue
#               gs_auth(new_user = TRUE)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
gd_modelrooster <- gs_title(config$cz_modelrooster_gd_reg)

modelrooster <- gd_modelrooster %>% 
  gs_read(ws = "modelrooster-20190620") 

moro_clean1 <- modelrooster %>%
  select(-X1,
         -A,
         -B,
         -Balk,
         -Toon,
         -matches("^(b|r|te|t\\d)", ignore.case = F)
         ) %>%
  rename(hh_offset = `hhOffset-dag.uur`) 
     # %>% mutate(dag = factor(x = dag, levels = c("do1", "vr1", "za1", "zo1", "ma1", "di1", "wo1", "do2"), ordered = T))

moro_long <-
  gather(
    data = moro_clean1,
    `elke week`,
    `week 1`,
    `week 2`,
    `week 3`,
    `week 4`,
    `week 5`,
    `twee-wekelijks`,
    key = "weken",
    value = "titel"
  ) %>% 
  filter(!is.na(titel)) %>% 
  mutate(
    cz_moro_slot = paste0(str_sub(dag, 1, 2), str_sub(start, 1, 2)),
    cz_moro_slot_len = as.integer(str_sub(start, 4, 6)),
    titel = sub("(.*)(?: S\\d)$", "\\1", titel, perl = TRUE)
  ) %>%
  arrange(cz_moro_slot, weken)

cz_week_start <- ymd_hms("2019-06-06 13:00:00")

cz_cal <- seq(cz_week_start, length.out = 13 * 168, by = "hours") %>% as_tibble()

names(cz_cal) <- "cz_cal_datetime"

cz_cal %<>% mutate(
  cz_cal_slot = format(cz_cal_datetime, format = "%a%H"),
  moro_week = case_when(
    day(cz_cal_datetime) > 28 ~ "week 5|elke week|twee-wekelijks",
    day(cz_cal_datetime) > 21 ~ "week 4|elke week|twee-wekelijks",
    day(cz_cal_datetime) > 14 ~ "week 3|elke week|twee-wekelijks",
    day(cz_cal_datetime) >  7 ~ "week 2|elke week|twee-wekelijks",
    TRUE  ~ "week 1|elke week|twee-wekelijks"
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
  select(titel) %>% unique

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

# cz_week_greep <- cz_week %>% filter(str_detect(titel, pattern = "Highw"))

# apple_scripts - prep voor aanmaken iTunes playlists ---------------------

gd_itunes_cupboard <- gs_title("iTunes cupboard")

itunes_cupboard_stage <- gd_itunes_cupboard %>% 
  gs_read(ws = "playlist_names")

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
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% # Joining, by = "titel"
  filter(uitzendmac_jn == "n") %>% 
  filter(!(titel == "Thema" & hour(cz_tijdstip) == 21)) %>% 
  select(-starts_with("week"), -uitzendmac_jn) %>% 
  mutate(sched_playlist = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>%
  arrange(titel)

cur_cz_week_uzm_prep <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% # Joining, by = "titel"
  filter(uitzendmac_jn == "j") %>% 
  filter(!(titel == "Thema" & hour(cz_tijdstip) == 21)) %>% 
  select(-starts_with("week"), -uitzendmac_jn) %>% 
  mutate(sched_playlist = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>%
  arrange(titel)

gd_montage <- gs_title("Roosters 3.0")

montage_stage <- gd_montage %>% 
  gs_read(ws = "montage") %>% 
  select(uzd, `Tijd.Duur`, Type)

montage <- montage_stage %>% 
  mutate(cz_tijdstip = ymd_h(paste0(uzd, " ", str_sub(`Tijd.Duur`, 1, 2))),
         mtr_live_jn = if_else(str_to_lower(Type) == "live", "j", "n")) %>% 
  select(cz_tijdstip, mtr_live_jn)

# huidige week maken ------------------------------------------------------

cur_cz_week_lgm <- cur_cz_week_lgm_prep %>% 
  left_join(montage, by = c("hh_van" = "cz_tijdstip")) %>% 
  rename(hh_van_live_jn = mtr_live_jn) %>% 
  left_join(montage) %>%  # Joining, by = "cz_tijdstip"
  rename(nieuw_live_jn = mtr_live_jn) 
  
cur_cz_week_uzm <- cur_cz_week_uzm_prep %>% 
  left_join(montage, by = c("hh_van" = "cz_tijdstip")) %>% 
  rename(hh_van_live_jn = mtr_live_jn) %>% 
  left_join(montage) %>%  # Joining, by = "cz_tijdstip"
  rename(nieuw_live_jn = mtr_live_jn) 

# live-programma's hebben geen script of playlist nodig

cur_cz_week_uzm %<>% 
  mutate(sched_playlist = if_else(nieuw_live_jn == "j", "CZ All Empty", sched_playlist))
