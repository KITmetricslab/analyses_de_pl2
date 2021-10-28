# read in daily truth data for plots_data2.R, put into separate file
# this generates rolling-window 7-day incidence time series to be used for plotting

# RKI, Germany:

# cases:
# read in data
dat_ecdc_gm_inc_case <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Cases_Germany.csv"),
                                 colClasses = list(date = "Date"))
# subset to Germany
dat_ecdc_gm_inc_case <- dat_ecdc_gm_inc_case[dat_ecdc_gm_inc_case$location == "GM",
                                             c("date", "location", "value")]
# adapt column names
colnames(dat_ecdc_gm_inc_case) <- c("date", "location", "inc_case")

# deaths:
dat_ecdc_gm_inc_death <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Deaths_Germany.csv"),
                                  colClasses = list(date = "Date"))
dat_ecdc_gm_inc_death <- dat_ecdc_gm_inc_death[dat_ecdc_gm_inc_death$location == "GM",
                                               c("date", "location", "value")]
colnames(dat_ecdc_gm_inc_death) <- c("date", "location", "inc_death")

# merge cases and deaths:
dat_ecdc_gm <- merge(dat_ecdc_gm_inc_case, dat_ecdc_gm_inc_death, by = c("date", "location"))
# apply rolling 7-day sum
dat_ecdc_gm$inc_case <- rollapply(dat_ecdc_gm$inc_case, 7, sum, fill = list(NA, NA, NA), align = "right")
dat_ecdc_gm$inc_death <- rollapply(dat_ecdc_gm$inc_death, 7, sum, fill = list(NA, NA, NA), align = "right")


# JHU, Germany:
dat_jhu_gm_inc_case <- read.csv(paste0(path_hub, "/data-truth/JHU/truth_JHU-Incident Cases_Germany.csv"),
                                colClasses = list(date = "Date"))
dat_jhu_gm_inc_case <- dat_jhu_gm_inc_case[dat_jhu_gm_inc_case$location == "GM",
                                           c("date", "location", "value")]
colnames(dat_jhu_gm_inc_case) <- c("date", "location", "inc_case")

dat_jhu_gm_inc_death <- read.csv(paste0(path_hub, "/data-truth/JHU/truth_JHU-Incident Deaths_Germany.csv"),
                                 colClasses = list(date = "Date"))
dat_jhu_gm_inc_death <- dat_jhu_gm_inc_death[dat_jhu_gm_inc_death$location == "GM",
                                             c("date", "location", "value")]
colnames(dat_jhu_gm_inc_death) <- c("date", "location", "inc_death")

dat_jhu_gm <- merge(dat_jhu_gm_inc_case, dat_jhu_gm_inc_death, by = c("date", "location"))
dat_jhu_gm$inc_case <- rollapply(dat_jhu_gm$inc_case, 7, sum, fill = list(NA, NA, NA), align = "right")
dat_jhu_gm$inc_death <- rollapply(dat_jhu_gm$inc_death, 7, sum, fill = list(NA, NA, NA), align = "right")


# MZ, Poland:
dat_ecdc_pl_inc_case <- read.csv(paste0(path_hub, "/data-truth/MZ/truth_MZ-Incident Cases_Poland.csv"),
                                 colClasses = list(date = "Date"))
dat_ecdc_pl_inc_case <- dat_ecdc_pl_inc_case[dat_ecdc_pl_inc_case$location == "PL",
                                             c("date", "location", "value")]
colnames(dat_ecdc_pl_inc_case) <- c("date", "location", "inc_case")

dat_ecdc_pl_inc_death <- read.csv(paste0(path_hub, "/data-truth/MZ/truth_MZ-Incident Deaths_Poland.csv"),
                                  colClasses = list(date = "Date"))
dat_ecdc_pl_inc_death <- dat_ecdc_pl_inc_death[dat_ecdc_pl_inc_death$location == "PL",
                                               c("date", "location", "value")]
colnames(dat_ecdc_pl_inc_death) <- c("date", "location", "inc_death")

dat_ecdc_pl <- merge(dat_ecdc_pl_inc_case, dat_ecdc_pl_inc_death, by = c("date", "location"))
dat_ecdc_pl$inc_case <- rollapply(dat_ecdc_pl$inc_case, 7, sum, fill = list(NA, NA, NA), align = "right")
dat_ecdc_pl$inc_death <- rollapply(dat_ecdc_pl$inc_death, 7, sum, fill = list(NA, NA, NA), align = "right")

# JHU, Poland:
dat_jhu_pl_inc_case <- read.csv(paste0(path_hub, "/data-truth/JHU/truth_JHU-Incident Cases_Poland.csv"),
                                colClasses = list(date = "Date"))
dat_jhu_pl_inc_case <- dat_jhu_pl_inc_case[dat_jhu_pl_inc_case$location == "PL",
                                           c("date", "location", "value")]
colnames(dat_jhu_pl_inc_case) <- c("date", "location", "inc_case")

dat_jhu_pl_inc_death <- read.csv(paste0(path_hub, "/data-truth/JHU/truth_JHU-Incident Deaths_Poland.csv"),
                                 colClasses = list(date = "Date"))
dat_jhu_pl_inc_death <- dat_jhu_pl_inc_death[dat_jhu_pl_inc_death$location == "PL",
                                             c("date", "location", "value")]
colnames(dat_jhu_pl_inc_death) <- c("date", "location", "inc_death")

dat_jhu_pl <- merge(dat_jhu_pl_inc_case, dat_jhu_pl_inc_death, by = c("date", "location"))
dat_jhu_pl$inc_case <- rollapply(dat_jhu_pl$inc_case, 7, sum, fill = list(NA, NA, NA), align = "right")
dat_jhu_pl$inc_death <- rollapply(dat_jhu_pl$inc_death, 7, sum, fill = list(NA, NA, NA), align = "right")


dat_ecdc <- rbind(dat_ecdc_gm, dat_ecdc_pl)
dat_jhu <- rbind(dat_jhu_gm, dat_jhu_pl)
