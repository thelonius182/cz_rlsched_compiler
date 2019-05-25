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
  gs_read(ws = "modelrooster-concept-R4.0") 

moroo1 <- modelrooster %>% 
  rename(hh_offset = `hhOffset-dag.uur`) %>% 
  filter(!is.na(hh_offset), hh_offset != 'tw') %>% 
  mutate(slot_start = paste0(str_sub(dag, start = 1, end = 2), str_sub(start, start = 1, end = 2)),
         slot_stop = str_sub(hh_offset, 1, 6)) %>% 
  select(slot_start, slot_stop)
  
peildtm_do <- ymd_hms('2019-05-23 00:00:00') # do
peildtm_vr <- ymd_hms('2019-05-24 00:00:00') # vr
peildtm_za <- ymd_hms('2019-05-25 00:00:00') # za
peildtm_zo <- ymd_hms('2019-05-26 00:00:00') # zo
peildtm_ma <- ymd_hms('2019-05-27 00:00:00') # ma
peildtm_di <- ymd_hms('2019-05-28 00:00:00') # di
peildtm_wo <- ymd_hms('2019-05-29 00:00:00') # wo

moroo2 <- moroo1 %>%
  mutate(
    slot_start_ts = case_when(
      str_detect(slot_start, 'do') ~ peildtm_do + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'vr') ~ peildtm_vr + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'za') ~ peildtm_za + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'zo') ~ peildtm_zo + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'ma') ~ peildtm_ma + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'di') ~ peildtm_di + hours(as.integer(str_sub(slot_start, 3, 4))),
      str_detect(slot_start, 'wo') ~ peildtm_wo + hours(as.integer(str_sub(slot_start, 3, 4)))
    ),
    slot_stop_ts = case_when(
      str_detect(slot_stop, 'do') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'vr') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'za') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'zo') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'ma') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'di') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2))),
      str_detect(slot_stop, 'wo') ~ slot_start_ts + days(as.integer(str_sub(slot_stop, 1, 2)))
    )
  )

hour(moroo2$slot_stop_ts) = as.integer(str_sub(moroo2$slot_stop, 5, 6))

