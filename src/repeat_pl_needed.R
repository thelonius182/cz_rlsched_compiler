moroo1 <- modelrooster %>% 
  rename(hh_offset = `hhOffset-dag.uur`) %>% 
  filter(!is.na(hh_offset), hh_offset != 'tw') %>% 
  mutate(slot_start = paste0(str_sub(dag, start = 1, end = 2), str_sub(start, start = 1, end = 2)),
         slot_stop = str_sub(hh_offset, 1, 6)) %>% 
  select(slot_start, slot_stop)

peildtm_do <- ymd_hms('2019-05-23 00:00:00') 
peildtm_vr <- ymd_hms('2019-05-24 00:00:00') 
peildtm_za <- ymd_hms('2019-05-25 00:00:00') 
peildtm_zo <- ymd_hms('2019-05-26 00:00:00') 
peildtm_ma <- ymd_hms('2019-05-27 00:00:00') 
peildtm_di <- ymd_hms('2019-05-28 00:00:00') 
peildtm_wo <- ymd_hms('2019-05-29 00:00:00') 

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

moroo3 <- moroo2 %>%
  mutate(
    needs_repeat_pl = ymd_hms('2019-05-30 13:00:00') %within% interval(slot_start_ts, slot_stop_ts)
  )

moroo4 <- moroo3 %>% 
  filter(needs_repeat_pl)
