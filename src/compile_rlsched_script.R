cur_cz_week_lgm <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% 
  filter(uitzend_mac_jn == "n") %>% 
  filter(titel != "Thema" | hour(cz_tijdstip) != 21) %>% 
  select(-starts_with("week"), -uitzend_mac_jn) %>% 
  mutate(sel_pl = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>% 
  arrange(cz_tijdstip)

cur_cz_week_uzm <- cz_week %>% 
  filter(weekschema == ymd_hm("2019-06-20 13:00")) %>% 
  inner_join(itunes_cupboard) %>% 
  filter(uitzend_mac_jn == "j") %>% 
  select(-starts_with("week"), -uitzend_mac_jn) %>% 
  mutate(sel_pl = if_else(is.na(hh_van) | is.na(playlist_hh), titel, playlist_hh)) %>% 
  arrange(cz_tijdstip)
