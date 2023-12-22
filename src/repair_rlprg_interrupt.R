pacman::p_load(readr, dplyr, stringr, fs, lubridate, futile.logger)

f1 <- flog.appender(appender.file("/Users/nipper/Logs/rlprg_interrupt_check.log"), name = "ric_log")
flog.info("
= = = = = Check & Fix .rlprg-interrupts (version 2023-12-22 13:40) = = = = =", name = "ric_log")

oe_list <-
  dir_ls(
    "//UITZENDMAC-2/macOS/Users/tech_1/Music/Radiologik/Programs",
    recurse = F,
    type = "file",
    regexp = "ochtendeditie"
  )

limit_ymd = now() - months(6) 
oe_list.2 <- oe_list |> as_tibble() |> mutate(oe_ymd_chr = str_extract(value, "\\d{8}"),
                                              oe_ymd = ymd(oe_ymd_chr)) |> filter(oe_ymd > limit_ymd) |> arrange(desc(value))
any_fix <- F

for (qfn in oe_list.2$value) {
  # qfn <- "//UITZENDMAC-2/macOS/Users/tech_1/Music/Radiologik/Programs/20231206_wo07-180_ochtendeditie_c_wo.rlprg"
  rlprg <- read_lines(qfn)
  dur <- rlprg[1] |> str_extract(pattern = "Duration:(.*)", group = 1)
  start_sec <- rlprg[2] |> str_extract(pattern = "\\tFALSE\\t(.*?)\\t", group = 1)
  fn <- path_file(qfn)
  
  if (is.na(dur) || is.na(start_sec) || dur != "10800" || start_sec != "25200") {
    any_fix <- T
    log_msg <- paste0("Fixing duration and/or interrupt of ", fn)
    flog.info(log_msg, name = "ric_log")
    rlprg.1 <- "Duration:10800"
    rlprg.2 <- sub("\\tFALSE\\t(.*?)\\t", "\tFALSE\t25200\t", rlprg[2], perl=TRUE, ignore.case=TRUE)
    rlprg[1] <- rlprg.1
    rlprg[2] <- rlprg.2
    write_lines(rlprg, qfn)
  }
}

if (!any_fix) {
  flog.info("No files needed fixing.", name = "ric_log")
}

flog.info("Job finished.", name = "ric_log")
