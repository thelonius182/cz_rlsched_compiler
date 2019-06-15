cur_cz_week_uzm_tsv <- cur_cz_week_uzm %>% 
  mutate(cz_van = format(cz_tijdstip, format = "%Y%m%d_%H"),
         cz_tot = format(cz_tijdstip + minutes(cz_slot_len), format = "%Y%m%d_%H"),
         mac = "U") %>% 
  select(cz_van, cz_tot, mac, sched_playlist) 

cur_cz_week_lgm_tsv <- cur_cz_week_lgm %>% 
  mutate(cz_van = format(cz_tijdstip, format = "%Y%m%d_%H"),
         cz_tot = format(cz_tijdstip + minutes(cz_slot_len), format = "%Y%m%d_%H"),
         mac = "L") %>% 
  select(cz_van, cz_tot, mac, sched_playlist)  

cur_cz_week_tsv <- rbind(cur_cz_week_uzm_tsv, cur_cz_week_lgm_tsv) %>% 
  arrange(cz_van)

write_delim(cur_cz_week_tsv, delim = "\t", col_names = T, path = "cz_week.tsv", append = F)

r4_corr_hijack <- montage %>% 
  mutate(hh_van = format(cz_tijdstip, "%Y%m%d_%H")) %>% 
  select(-cz_tijdstip)

r4_corr <- gs_title("R4-correcties")

r4_corr_stage <- r4_corr %>% 
  gs_read(ws = "corr_full") %>% 
  select(`hh-van`) %>%
  rename(hh_van = `hh-van`) %>% 
  filter(!hh_van == "-") %>% 
  left_join(r4_corr_hijack) %>% 
  filter(mtr_live_jn == "j")
