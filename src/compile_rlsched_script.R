cur_cz_week_uzm <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% 
  filter(uitzend_mac_jn == "n") %>% 
  select(-starts_with("week")) %>% 
  arrange(cz_tijdstip)
