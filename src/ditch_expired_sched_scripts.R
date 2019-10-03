get_weekfiles_to_ditch <- function(cz_dir_path) {
  
  script_rgx <- ".*Schedule/[0-9]{3} - ([0-9]{4}-[0-9]{2}-[0-9]{2})_\\w{2}([0-9]{2})_([0-9]{3}).*"
  
  czweek_sched <- dir_info(path = cz_dir_path) %>% 
    rename(dir_info_path = path) %>% 
    filter(str_detect(dir_info_path, "^.*? - [0-9]{4}-.*")) %>%
    mutate(script_date = sub(script_rgx, "\\1", dir_info_path, perl=TRUE),
           script_hour = sub(script_rgx, "\\2", dir_info_path, perl=TRUE),
           script_length = sub(script_rgx, "\\3", dir_info_path, perl=TRUE)) %>% 
    select(dir_info_path, starts_with("script"))
  
  czweek_expiration <- czweek_sched %>% 
    mutate(script_ymdh_s = paste0(script_date, " ", script_hour, ":00"),
           script_ymdh = ymd_hm(script_ymdh_s, tz = "Europe/Amsterdam"),
           # script verloopt 1 uur na einde uitzending
           script_expires = script_ymdh + minutes(as.integer(script_length) + 60),
           script_interval = interval(script_expires, now(), tz = "Europe/Amsterdam"),
           expired_h = round(script_interval / dhours(1), digits = 1)
    )
  
  czweek_files_to_ditch <- czweek_expiration %>% 
    select(-starts_with("script")) %>% # pre-select needed to loose var 'interval': 'filter' can't handle that
    filter(expired_h > 0.0) %>% 
    select(dir_info_path)
  
  return(czweek_files_to_ditch)
}

resequence_script_names <- function(cz_dir_path) {
  rl_sched_flr <- dir_info(path = cz_dir_path) %>% 
    select(dir_info_path = path) %>% 
    mutate(
      rls_index = sprintf("%03d", row_number()),
      dir_info_path_new = fs_path(str_replace(dir_info_path, 
                                      pattern = "/[0-9]{3} - ", 
                                      replacement = paste0("/", rls_index, " - ")
                                  )
                          )
    ) %>% 
    select(-rls_index) %>% 
    filter(dir_info_path != dir_info_path_new)

  file_move(path = rl_sched_flr$dir_info_path, new_path = rl_sched_flr$dir_info_path_new)  
}
