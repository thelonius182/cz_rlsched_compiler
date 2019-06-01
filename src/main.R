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
  rename(hh_offset = `hhOffset-dag.uur`) %>% 
  mutate(dag = factor(x = dag, levels = c("do1", "vr1", "za1", "zo1", "ma1", "di1", "wo1", "do2"), ordered = T))

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
# <<<<<<< HEAD
#   mutate(cz_slot = paste0(str_sub(dag, 1, 2), str_sub(start, 1, 2))) %>% 
#   arrange(cz_slot, weken)
# 
# cz_week_start <- ymd_hms("2019-06-06 13:00:00")
# 
# cz_cal <- seq(cz_week_start, length.out = 168, by = "hours") %>% as_tibble()
# =======
  mutate(cz_slot_orig = paste0(str_sub(dag, 1, 2), str_sub(start, 1, 2))) %>% 
  arrange(cz_slot_orig, weken)

cz_cal <- seq(ymd_hms("2019-06-06 13:00:00"), length.out = 168, by = "hours") %>% as_tibble()
# >>>>>>> 796e9255fef8c4d522f7946843372770ceccee74
names(cz_cal) <- "cz_dt"
cz_cal_slot <- cz_cal %>%
  mutate(
    cz_slot = format(cz_dt, format = "%a%H"),
    moro_week = case_when(
      day(cz_dt) > 28 ~ "week 5|elke week|twee-wekelijks",
      day(cz_dt) > 21 ~ "week 4|elke week|twee-wekelijks",
      day(cz_dt) > 14 ~ "week 3|elke week|twee-wekelijks",
      day(cz_dt) >  7 ~ "week 2|elke week|twee-wekelijks",
      TRUE  ~ "week 1|elke week|twee-wekelijks"
    )
  )

# <<<<<<< HEAD
# cz_cal_slot_moro <- inner_join(cz_cal_slot, moro_long) %>%
#   filter(str_detect(string = weken, pattern = moro_week))
# 
# cz_cal_slot_moro_hh <- cz_cal_slot_moro %>%
#   mutate(
#     hh_1 = str_sub(hh_offset, 1, 2),
#     # hh_inc_dagen = if_else(hh_1 == "tw", 7L, as.integer(hh_1)),
#     hh_inc_dagen = as.integer(hh_1),
#     cz_dt_hh = cz_dt + days(hh_inc_dagen),
#     # uu_1 = if_else(hh_offset == "tw", str_sub(cz_slot, 3, 4), str_sub(hh_offset, 5, 6)),
#     uu_1 = str_sub(hh_offset, 5, 6)
#   )
# hour(cz_cal_slot_moro_hh$cz_dt_hh) <- as.integer(cz_cal_slot_moro_hh$uu_1) 
# 
# 
# # weken samenstellen ------------------------------------------------------
# 
# week_orig <- cz_cal_slot_moro_hh %>% 
#   rename(cz_tijdstip = cz_dt) %>% 
#   select(cz_tijdstip, titel) %>% 
#   mutate(hh_van = NA)
# 
# week_herh <- cz_cal_slot_moro_hh %>% 
#   rename(cz_tijdstip = cz_dt_hh, hh_van = cz_dt) %>% 
#   select(cz_tijdstip, titel, hh_van) %>% 
#   filter(!is.na(cz_tijdstip))
# 
# cz_week <- bind_rows(week_orig, week_herh) %>% 
#   mutate(titel = sub("(.*)(?: S\\d)$", "\\1", titel, perl=TRUE)) %>% 
#   arrange(cz_tijdstip)
# =======
cz_cal_slot_moro <- inner_join(cz_cal_slot, moro_long) %>% 
    filter(str_detect(string = weken, pattern = moro_week))
# >>>>>>> 796e9255fef8c4d522f7946843372770ceccee74
