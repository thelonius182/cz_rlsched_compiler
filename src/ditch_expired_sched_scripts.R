get_weekfiles_to_ditch <- function(cz_dir_path) {
  # TEST 
  # cz_dir_path = "//UITZENDMAC-CZ/Radiologik/Schedule"
  # TEST 
  
  # script_rgx <- ".*Schedule/[0-9]{3} - ([0-9]{4}-[0-9]{2}-[0-9]{2})_\\w{2}([0-9]{2})_([0-9]{3}).*"
  
  czweek_sched <- dir_info(path = cz_dir_path) %>% 
    mutate(script_item = sub(".*?/Schedule/(\\d{3}).*", "\\1", path, perl=TRUE),
           script_date = sub(".*?/Schedule/\\d{3} - ([-0-9]{10})_.*", "\\1", path, perl=TRUE),
           script_hour = sub(".*?/Schedule/\\d{3} - [-0-9]{10}_\\w{2}(\\d{2}).*", "\\1", path, perl=TRUE),
           script_length = sub(".*?/Schedule/.*\\w{2}\\d{2}[-_](\\d{3}).*", "\\1", path, perl=TRUE)
    ) %>% 
    filter(str_starts(script_date, "[0-9]")) %>%
    select(path, starts_with("script"))
  
  czweek_expiration <- czweek_sched %>% 
    mutate(script_dts_chr = paste0(script_date, " ", script_hour, "00:00"),
           script_dts = ymd_hms(script_dts_chr, tz = "Europe/Amsterdam"),
           # script verloopt 1 uur na einde uitzending
           script_expires = script_dts + dminutes(as.integer(script_length) + 60L),
           script_interval = interval(script_expires, now(), tz = "Europe/Amsterdam"),
           expired_h = int_length(script_interval) / 3600
    )
  
  czweek_files_to_ditch <- czweek_expiration %>% 
    select(-starts_with("script")) %>% # pre-select needed to loose var 'interval': 'filter' can't handle that
    filter(expired_h > 0.0) %>% 
    select(path)
  
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
