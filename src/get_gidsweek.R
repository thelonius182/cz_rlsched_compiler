library(readtext)
library(stringi)

flog.appender(appender.file("/Users/nipper/Logs/rlsched_compiler.log"), name = "rlsc_log")

# define wordpress connection ---------------------------------------------

get_wp_conn <- function() {
  db_type <- "prd"
  db_host <- key_get(service = paste0("sql-wp", db_type, "_host"))
  db_user <- key_get(service = paste0("sql-wp", db_type, "_user"))
  db_password <- key_get(service = paste0("sql-wp", db_type, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", db_type, "_db"))
  db_port <- 3306
  result <- tryCatch( {
    grh_conn <- dbConnect(drv = MySQL(), user = db_user, password = db_password,
                          dbname = db_name, host = db_host, port = db_port)
  },
  error = function(cond) {
    flog.error("Wordpress database onbereikbaar (1)", name = "rlsc_log")
    return("connection-error")
  }
  )
  return(result)
}

# create plws-gidsweek ----------------------------------------------------

for (seg1 in 1:1) { # creates an exitable flow
  # Connect to database 
  wp_conn <- get_wp_conn()
  
  # connection type S4 indicates a valid connection; other types indicate failure
  if (typeof(wp_conn) != "S4") { 
    flog.error("Wordpress database onbereikbaar (2)", name = "rlsc_log")
    break
  }
  
  # replace table plws_gidsweek
  flog.info("Starting sql-cmd: replace gidsweek.", name = "rlsc_log")
  dbExecute(wp_conn, "drop table if exists salsa_plws_gidsweek;")
  
  # build sql-cmd gidsweek 
  plws_dt_fmt <- stamp("1958-12-25 00:53")
  plws_gidsweek_end <- plws_dt_fmt(ymd_hm(config$cur_cz_week) + days(7))
  
  sql_cmd_template <- readtext(file = "resources/sql_cmd_get_gidsweek.txt", encoding = "UTF-8")
  
  sql_cmd <-
    stri_replace_all_fixed(
      str = sql_cmd_template$text,
      pattern = "$#WEEK_START",
      replacement = paste0("'", config$cur_cz_week, "'")
    )
  
  sql_cmd <-
    stri_replace_all_fixed(
      str = sql_cmd,
      pattern = "$#WEEK_STOP",
      replacement = paste0("'", plws_gidsweek_end, "'")
    )
  
  # execute it and put result into tibble, for use in compile_pl_wk_schema.R
  dbExecute(wp_conn, sql_cmd)
  
  flog.info("gidsweek replaced.", name = "rlsc_log")
  
  salsa_plws_gidsweek <- dbGetQuery(wp_conn, "select cz_id,
                                              substr(pgmStart, 1, 8) as pgm_dtm,
                                              substr(pgmStart, 10) as pgm_start,
                                              substr(pgmStop, 10) as pgm_stop,
                                              pgmPostTitle as title,
                                              herh_van,
                                              cz_id_herh
                                              from salsa_plws_gidsweek
                                              order by pgmStart, pgmLang desc;"
  )

  on.exit(dbDisconnect(wp_conn))
}
