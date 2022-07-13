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
  flog.info("Vervang de gidsweek: fase 1 van 2 loopt", name = "rlsc_log")
  dbExecute(wp_conn, "drop table if exists salsa_plws_gidsweek;")
  
  # build sql-cmd gidsweek 
  plws_dt_fmt <- stamp("1958-12-25 00:53")
  plws_gidsweek_end <- plws_dt_fmt(ymd_hm(config$cur_cz_week) + days(7))
  
  sql_cmd_template <- 
    "create temporary table salsa_plws_aux_1 as
SELECT 
    po1.id AS cz_id,
    po1.post_title as pgmPostTitle,
    DATE_FORMAT(po1.post_date, '%Y%m%d_%H') AS pgmStart
FROM
    wp_posts po1
        JOIN
    wp_term_relationships tr1 ON tr1.object_id = po1.id
WHERE
    po1.post_date >= #WEEK_START
        AND po1.post_date < #WEEK_STOP
        AND tr1.term_taxonomy_id = 5
        AND po1.post_type = 'programma'
        AND po1.post_status = 'publish'
ORDER BY 2"
  
  sql_cmd <-
    str_replace(
      string = sql_cmd_template,
      pattern = "#WEEK_START",
      replacement = paste0("'", config$cur_cz_week, "'")
    )
  
  sql_cmd <-
    str_replace(
      string = sql_cmd,
      pattern = "#WEEK_STOP",
      replacement = paste0("'", plws_gidsweek_end, "'")
    )
  
  # execute it and put result into tibble, for use in compile_pl_wk_schema.R
  dbExecute(wp_conn, sql_cmd)
  dbExecute(wp_conn, "ALTER TABLE salsa_plws_aux_1 ADD INDEX (cz_id)")
  
  sql_cmd <- 
    "create temporary table salsa_plws_aux_2 as
SELECT 
    pa1.cz_id,
    DATE_FORMAT(pm3.meta_value, '%Y%m%d_%H') AS pgmStop
FROM
    salsa_plws_aux_1 pa1
        JOIN
    wp_postmeta pm3 ON pm3.post_id = pa1.cz_id
WHERE
    pm3.meta_key = 'pr_metadata_uitzenddatum_end'
ORDER BY 1"
  dbExecute(wp_conn, sql_cmd)
  dbExecute(wp_conn, "ALTER TABLE salsa_plws_aux_2 ADD INDEX (cz_id)")
  
  sql_cmd <- 
    "create temporary table salsa_plws_aux_3 as
SELECT 
    pa1.cz_id,
    case when pm2.meta_value is not null and char_length(trim(pm2.meta_value)) > 0
            then cast(pm2.meta_value as unsigned) 
		 else -1
	end as cz_id_herh
FROM
    salsa_plws_aux_1 pa1
        left JOIN
    wp_postmeta pm2 ON pm2.post_id = pa1.cz_id
WHERE
    pm2.meta_key = 'pr_metadata_orig'
ORDER BY 1"
  dbExecute(wp_conn, sql_cmd)
  dbExecute(wp_conn, "ALTER TABLE salsa_plws_aux_3 ADD INDEX (cz_id)")
  
  sql_cmd <- 
    "create temporary table salsa_plws_aux_4 as
SELECT 
    pa3.cz_id_herh,
    po1.post_title as pgmPostTitle_herh,
    DATE_FORMAT(po1.post_date, '%Y%m%d_%H') as herh_van
FROM
    salsa_plws_aux_3 pa3
        JOIN
    wp_posts po1 ON po1.id = pa3.cz_id_herh
WHERE
    pa3.cz_id_herh > 0
ORDER BY 1"
  dbExecute(wp_conn, sql_cmd)
  dbExecute(wp_conn, "ALTER TABLE salsa_plws_aux_4 ADD INDEX (cz_id_herh)")
  
  sql_cmd <- 
    "create table salsa_plws_gidsweek as
     select distinct pgmStart,
       pgmStop,
       case when pgmPostTitle_herh is not null then pgmPostTitle_herh
            else pgmPostTitle
	   end as pgmPostTitle,
       case when herh_van is not null then herh_van
            else '-'
	   end as herh_van,
       case when t3.cz_id_herh is not null then t3.cz_id_herh
            else '-'
	   end as cz_id_herh,
       t1.cz_id,
       5 as pgmLang
from salsa_plws_aux_1 t1 left join (salsa_plws_aux_3 t3 join salsa_plws_aux_4 t4 
                                                          on t3.cz_id_herh = t4.cz_id_herh)
                                on t1.cz_id = t3.cz_id 
                         join salsa_plws_aux_2 t2
                           on t1.cz_id = t2.cz_id
order by 1"
  dbExecute(wp_conn, sql_cmd)
  
  flog.info("Vervang de gidsweek: fase 2 van 2 loopt", name = "rlsc_log")
  
  salsa_plws_gidsweek <- dbGetQuery(wp_conn, "select cz_id,
                                              substr(pgmStart, 1, 8) as pgm_dtm,
                                              substr(pgmStart, 10) as pgm_start,
                                              substr(pgmStop, 10) as pgm_stop,
                                              pgmPostTitle as title,
                                              herh_van,
                                              cz_id_herh
                                              from salsa_plws_gidsweek
                                              order by pgmStart, pgmLang desc")
  
  flog.info("Vervang de gidsweek: voltooid", name = "rlsc_log")

  on.exit(dbDisconnect(wp_conn))
}
