# Generate summary tables on results, pooled across study periods

# set language to English (Unix systems)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

library(xtable)
library(here)
# setwd("/home/johannes/Documents/Projects/second_period/R")
source("define_models2.R")
source("define_colors2.R")
source("functions_paper2.R")


models <- unique(c(unlist(models_gm), unlist(models_pl)))

path_hub <- "../../covid19-forecast-hub-de"

# specify truth data (needs to be run once with "ECDC" and once with "JHU"):
truth <- "ECDC"

dat_evaluation <- read.csv(paste0(path_hub, "/evaluation/evaluation-", truth, ".csv"),
                           colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                           stringsAsFactors = FALSE)

dat_evaluation <- subset(dat_evaluation, model %in% models)
# also remove previous IHME forecasts

dat_additional_evaluation <- read.csv(paste0("../additional_forecasts/additional_evaluation-", truth, ".csv"),
                                      colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                                      stringsAsFactors = FALSE)

# join:
dat_evaluation <- rbind(dat_evaluation, dat_additional_evaluation)

# remove duplicates (some states were already in the original FIAS files):
inds_dupl <- duplicated(dat_evaluation[, c("model", "location", "timezero", "target_end_date", "target")])
dat_evaluation <- dat_evaluation[!inds_dupl, ]


# specify evaluation period:
first_forecast_date <- as.Date("2020-10-12")
last_forecast_date <- as.Date("2021-03-29")


# manually remove Christmas break from evaluation results data:
dat_evaluation <- subset(dat_evaluation,
                         !(timezero %in% as.Date(c("2020-12-21", "2020-12-28", "2021-01-04"))) &
                           !(target_end_date %in% as.Date(c("2020-12-26", "2021-01-02", "2021-01-09"))))

######################################
### National level
######################################

# Germany, cases:

## 1 + 2 wk
# undebug(generate_summary)
summary_gm_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", locations = "GM",
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_gm_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", locations = "GM",
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_gm_inc_case_12 <- restrict_summary(summary_gm_inc_case)
summary_gm_cum_case_12 <- restrict_summary(summary_gm_cum_case)

summary_gm_case_12 <- merge_inc_cum_summaries(summary_gm_inc_case_12,
                                              summary_gm_cum_case_12)

xtable_summary_tab(summary_gm_case_12)

summary_gm_inc_case_34 <- restrict_summary(summary_gm_inc_case, horizons = 3:4)
summary_gm_cum_case_34 <- restrict_summary(summary_gm_cum_case, horizons = 3:4)

summary_gm_case_34 <- merge_inc_cum_summaries(summary_gm_inc_case_34,
                                              summary_gm_cum_case_34)


## Germany, deaths:

## 1 + 2 wk
undebug(generate_summary)
summary_gm_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", locations = "GM",
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_gm_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", locations = "GM",
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_gm_inc_death_12 <- restrict_summary(summary_gm_inc_death)
summary_gm_cum_death_12 <- restrict_summary(summary_gm_cum_death)

summary_gm_death_12 <- merge_inc_cum_summaries(summary_gm_inc_death_12,
                                               summary_gm_cum_death_12)


summary_gm_inc_death_34 <- restrict_summary(summary_gm_inc_death, horizons = 3:4)
summary_gm_cum_death_34 <- restrict_summary(summary_gm_cum_death, horizons = 3:4)

summary_gm_death_34 <- merge_inc_cum_summaries(summary_gm_inc_death_34,
                                               summary_gm_cum_death_34)


summary_gm_inc_12 <- merge_case_death_summaries(summary_gm_inc_case_12, summary_gm_inc_death_12)
summary_gm_inc_34 <- merge_case_death_summaries(summary_gm_inc_case_34, summary_gm_inc_death_34)



# Poland, cases:

## 1 + 2 wk
summary_pl_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", locations = "PL",
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_pl_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", locations = "PL",
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_pl_inc_case_12 <- restrict_summary(summary_pl_inc_case)
summary_pl_cum_case_12 <- restrict_summary(summary_pl_cum_case)

summary_pl_case_12 <- merge_inc_cum_summaries(summary_pl_inc_case_12,
                                              summary_pl_cum_case_12)


summary_pl_inc_case_34 <- restrict_summary(summary_pl_inc_case, horizons = 3:4)
summary_pl_cum_case_34 <- restrict_summary(summary_pl_cum_case, horizons = 3:4)

summary_pl_case_34 <- merge_inc_cum_summaries(summary_pl_inc_case_34,
                                              summary_pl_cum_case_34)

## Poland, deaths:

## 1 + 2 wk
summary_pl_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", locations = "PL",
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_pl_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", locations = "PL",
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_pl_inc_death_12 <- restrict_summary(summary_pl_inc_death)
summary_pl_cum_death_12 <- restrict_summary(summary_pl_cum_death)

summary_pl_death_12 <- merge_inc_cum_summaries(summary_pl_inc_death_12,
                                               summary_pl_cum_death_12)


summary_pl_inc_death_34 <- restrict_summary(summary_pl_inc_death, horizons = 3:4)
summary_pl_cum_death_34 <- restrict_summary(summary_pl_cum_death, horizons = 3:4)

summary_pl_death_34 <- merge_inc_cum_summaries(summary_pl_inc_death_34,
                                               summary_pl_cum_death_34)


summary_pl_inc_12 <- merge_case_death_summaries(summary_pl_inc_case_12, summary_pl_inc_death_12)
summary_pl_inc_34 <- merge_case_death_summaries(summary_pl_inc_case_34, summary_pl_inc_death_34)


for(fil in c("summary_gm_case_12",
             "summary_gm_death_12",
             "summary_gm_case_34",
             "summary_gm_death_34",
             "summary_pl_case_12",
             "summary_pl_death_12",
             "summary_pl_case_34",
             "summary_pl_death_34",
             "summary_gm_inc_12",
             "summary_gm_inc_34",
             "summary_pl_inc_12",
             "summary_pl_inc_34")){
  writeLines(xtable_summary_tab(get(fil)), con = paste0("../input/", fil, "_", truth, "_pooled.tex"))
}

source("define_colors2.R")

######################################
### Subnational level
######################################

# subset to relevant models:
dat_evaluation_regions <- subset(dat_evaluation, model != "LeipzigIMISE-SECIR")

# Get location codes:
location_codes_gm <- read.csv(paste0(path_hub, "/template/state_codes_germany.csv"))
locations_gmregions <- location_codes_gm$state_code[nchar(location_codes_gm$state_code) == 4]

location_codes_pl <- read.csv(paste0(path_hub, "/template/state_codes_poland.csv"))
locations_plregions <- location_codes_pl$state_code[nchar(location_codes_pl$state_code) == 4]

# Germany, cases:

## 1 + 2 wk
# undebug(generate_summary)
summary_gmregions_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", locations = locations_gmregions,
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_gmregions_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", locations = locations_gmregions,
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_gmregions_inc_case_12 <- restrict_summary(summary_gmregions_inc_case)
summary_gmregions_cum_case_12 <- restrict_summary(summary_gmregions_cum_case)

summary_gmregions_case_12 <- merge_inc_cum_summaries(summary_gmregions_inc_case_12,
                                              summary_gmregions_cum_case_12)

summary_gmregions_inc_case_34 <- restrict_summary(summary_gmregions_inc_case, horizons = 3:4)
summary_gmregions_cum_case_34 <- restrict_summary(summary_gmregions_cum_case, horizons = 3:4)

summary_gmregions_case_34 <- merge_inc_cum_summaries(summary_gmregions_inc_case_34,
                                              summary_gmregions_cum_case_34)


## Germany, deaths:

## 1 + 2 wk
summary_gmregions_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", locations = locations_gmregions,
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_gmregions_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", locations = locations_gmregions,
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_gmregions_inc_death_12 <- restrict_summary(summary_gmregions_inc_death)
summary_gmregions_cum_death_12 <- restrict_summary(summary_gmregions_cum_death)

summary_gmregions_death_12 <- merge_inc_cum_summaries(summary_gmregions_inc_death_12,
                                               summary_gmregions_cum_death_12)


summary_gmregions_inc_death_34 <- restrict_summary(summary_gmregions_inc_death, horizons = 3:4)
summary_gmregions_cum_death_34 <- restrict_summary(summary_gmregions_cum_death, horizons = 3:4)

summary_gmregions_death_34 <- merge_inc_cum_summaries(summary_gmregions_inc_death_34,
                                               summary_gmregions_cum_death_34)


summary_gmregions_inc_12 <- merge_case_death_summaries(summary_gmregions_inc_case_12, summary_gmregions_inc_death_12)
summary_gmregions_inc_34 <- merge_case_death_summaries(summary_gmregions_inc_case_34, summary_gmregions_inc_death_34)



# Poland, cases:

## 1 + 2 wk
summary_plregions_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", locations = locations_plregions,
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_plregions_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", locations = locations_plregions,
                                        first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_plregions_inc_case_12 <- restrict_summary(summary_plregions_inc_case)
summary_plregions_cum_case_12 <- restrict_summary(summary_plregions_cum_case)

summary_plregions_case_12 <- merge_inc_cum_summaries(summary_plregions_inc_case_12,
                                              summary_plregions_cum_case_12)


summary_plregions_inc_case_34 <- restrict_summary(summary_plregions_inc_case, horizons = 3:4)
summary_plregions_cum_case_34 <- restrict_summary(summary_plregions_cum_case, horizons = 3:4)

summary_plregions_case_34 <- merge_inc_cum_summaries(summary_plregions_inc_case_34,
                                              summary_plregions_cum_case_34)

## Poland, deaths:

## 1 + 2 wk
summary_plregions_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", locations = locations_plregions,
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)

summary_plregions_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", locations = locations_plregions,
                                         first_forecast_date = first_forecast_date, last_forecast_date = last_forecast_date)


summary_plregions_inc_death_12 <- restrict_summary(summary_plregions_inc_death)
summary_plregions_cum_death_12 <- restrict_summary(summary_plregions_cum_death)

summary_plregions_death_12 <- merge_inc_cum_summaries(summary_plregions_inc_death_12,
                                               summary_plregions_cum_death_12)


summary_plregions_inc_death_34 <- restrict_summary(summary_plregions_inc_death, horizons = 3:4)
summary_plregions_cum_death_34 <- restrict_summary(summary_plregions_cum_death, horizons = 3:4)

summary_plregions_death_34 <- merge_inc_cum_summaries(summary_plregions_inc_death_34,
                                               summary_plregions_cum_death_34)


summary_plregions_inc_12 <- merge_case_death_summaries(summary_plregions_inc_case_12, summary_plregions_inc_death_12)
summary_plregions_inc_34 <- merge_case_death_summaries(summary_plregions_inc_case_34, summary_plregions_inc_death_34)


for(fil in c("summary_gmregions_case_12",
             "summary_gmregions_death_12",
             "summary_gmregions_case_34",
             "summary_gmregions_death_34",
             "summary_plregions_case_12",
             "summary_plregions_death_12",
             "summary_plregions_case_34",
             "summary_plregions_death_34",
             "summary_gmregions_inc_12",
             "summary_gmregions_inc_34",
             "summary_plregions_inc_12",
             "summary_plregions_inc_34")){
  writeLines(xtable_summary_tab(get(fil)), con = paste0("../input/", fil, "_", truth, "_2.tex"))
}

### plot of performance decay over horizons:

models_horizons_germany <- c("KITCOVIDhub-median_ensemble",
                             models_gm$main[models_gm$main != "KITCOVIDhub-median_ensemble"],
                             "KIT-extrapolation_baseline", "KIT-time_series_baseline")
models_horizons_poland <- c("KITCOVIDhub-median_ensemble",
                            models_pl$main[models_gm$main != "KITCOVIDhub-median_ensemble"],
                            "KIT-extrapolation_baseline", "KIT-time_series_baseline")


pdf("../figures/performance_horizons2.pdf", width = 9, height = 12)

par(mfrow = c(4, 1), mar = c(7.5, 4, 4, 2))

plot_performance_decay(summary_gm_inc_case, 
                       models = models_horizons_germany,
                       col = cols[models_horizons_germany], legend = TRUE, main = "Cases, Germany")

plot_performance_decay(summary_gm_inc_death, 
                       models = models_horizons_germany,
                       col = cols[models_horizons_germany], legend = TRUE, main = "Deaths, Germany")

plot_performance_decay(summary_pl_inc_case, 
                       models = models_horizons_poland,
                       col = cols[models_horizons_poland], legend = TRUE, main = "Cases, Poland")

plot_performance_decay(summary_pl_inc_death, 
                       models = models_horizons_poland,
                       col = cols[models_horizons_poland], legend = TRUE, main = "Deaths, Poland")

dev.off()
