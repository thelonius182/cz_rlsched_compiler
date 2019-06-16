# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Build applescript to create iTunes playlists
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

build_applescript_ipls <- function(ipl_tbl) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Example:
  # tell application "iTunes"
  # set cur_parent_folder to get parent of playlist "live_ref"
  # make user playlist at cur_parent_folder with properties {name:"test_lon10"}
  # set cur_parent_folder to get parent of playlist "semi_live_ref"
  # make user playlist at cur_parent_folder with properties {name:"test_lon20"}
  # end tell
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # testrun: 
  # ipl_tbl = ipl_lgm_live
  
  ascr01_C0 <- "tell application \"iTunes\"" %>% as_tibble
  ascr01_C1 <- "set cur_parent_folder to get parent of playlist \"r4_ref\"" %>% as_tibble
  # ascr01_C3 <- paste0(ascr01_C2a, ascr01_C2b, ascr01_C2c) %>% as_tibble
  ascr01_C4 <- "end tell" %>% as_tibble

  script_file <- bind_rows(ascr01_C0, 
                           ascr01_C1, 
                           build_ascr_lines(ipl_tbl),
                           ascr01_C4
  )
  write.table(x = script_file, file = "ascr.txt", row.names = FALSE, col.names = FALSE, 
              sep = " ", quote = FALSE, fileEncoding = "UTF-8") 
}

build_ascr_lines <- function(some_ipl_tbl) {
  fragm01_C2a <- "make user playlist at cur_parent_folder with properties {name:\""
  fragm01_C2c <- "\"}"
  ascr_lines <- ""
  
  for (ipl_name in some_ipl_tbl) {
    ascr_lines %<>% paste0(fragm01_C2a, ipl_name, fragm01_C2c) 
  }
  
  return(ascr_lines %>% as_tibble)
}
