# Generate plots of forecasts issued by teams and ensembles

# setwd("/home/johannes/Documents/Projects/second_period/R")

library(here)
library(plotrix)
library(colorspace)

# set language to English (Unix systems)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# source functions and colour definitions:
source("functions_paper2.R")
source("define_colors2.R")
source("define_models2.R")

# get evaluation + truth data and plotting functions from the Hub:
path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
source(paste0(path_hub, "/code/R/plot_functions.R"))

dat_eval <- read.csv(paste0(path_hub, "/evaluation/evaluation-ECDC.csv"),
                     colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                     stringsAsFactors = FALSE)
dat_eval <- subset(dat_eval, timezero >= as.Date("2021-01-11") & timezero <= as.Date("2021-03-29"))

dat_truth <- read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_ecdc.csv"),
                      colClasses = list(date = "Date"))
dat_truth_gm <- subset(dat_truth, location == "GM")
dat_truth_pl <- subset(dat_truth, location == "PL")


# define shifts for bands of multple foreasts in the same plot:
shifts <- c(-1.8, -1.2, -0.6, 0, 0.6, 1.2, 1.8)
x_start <- as.Date("2021-01-11")
x_end <- as.Date("2021-04-11")
# suppress scientific notation for plot labels:
options(scipen = 1000)


par(las = 1)

models_to_plot_gm <- list(main = c("epiforecasts-EpiExpert",
                                   "itwm-dSEIR",
                                   "LANL-GrowthRate",
                                   "FIAS_FZJ-Epi1Ger",
                                   "LeipzigIMISE-SECIR",
                                   "KIT-baseline",
                                   "KITCOVIDhub-median_ensemble"),
                          others = c("epiforecasts-EpiNow2",
                                     "Imperial-ensemble2",
                                     "ITWW-county_repro",
                                     "Karlen-pypm",
                                     "MIT_CovidAnalytics-DELPHI",
                                     "SDSC_ISG-TrendModel",
                                     "USC-SIkJalpha"))

models_to_plot_pl <- list(main = c("epiforecasts-EpiExpert",
                                   "LANL-GrowthRate",
                                   "MIMUW-StochSEIR",
                                   "ICM-agentModel",
                                   "MOCOS-agent1",
                                   "KIT-baseline",
                                   "KITCOVIDhub-median_ensemble"),
                          others = c("epiforecasts-EpiNow2",
                                     "Imperial-ensemble2",
                                     "ITWW-county_repro",
                                     "MIT_CovidAnalytics-DELPHI",
                                     "SDSC_ISG-TrendModel",
                                     "USC-SIkJalpha"))

# separate plots for "main" and "other" models (main manuscript and appendix)
for(model_cat in c("main", "others")){ #, "others"
  # run over horizons
  for(h in 1:4){
    # needed to plot coverage:
    dat_eval_list <- list(ECDC = dat_eval)
    
    pdf(paste0("../figures/plot_forecasts_", h, "wk_", model_cat,"_2.pdf"), width = 8, height = 10)
    par(mfrow = c(4, 1), las = 1)
    
    layout(matrix(1:8, ncol = 2, byrow = TRUE), widths = c(4, 1))
    
    # cases, DE
    plot_from_eval(dat_eval = dat_eval,
                   model = models_to_plot_gm[[model_cat]],
                   location = "GM", target_type = "case",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_to_plot_gm[[model_cat]]],
                   pch = pchs[models_to_plot_gm[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = "top", ylim = c(0, 300000), add_stars = TRUE)
    legend("topright", col = c("darkgrey", "lightgrey"), legend = c("50% PI", "95% PI"), pch = 15, bty = "n")
    legend("bottomright", col = "black", lty = 1, pch = 15, legend = "observed\n (ECDC)", cex = 0.9, bty = "n")
    lines(dat_truth_gm$date, dat_truth_gm$inc_case, type = "o", pch = 15)
    title(paste("(a) Weekly incident cases, Germany,", h, "wk ahead"))
    
    plot_scores(scores = dat_eval_list,
                target = "inc case",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_to_plot_gm[[model_cat]],
                location = "GM",
                start = x_start,
                end = x_end,
                cols = cols[models_to_plot_gm[[model_cat]]], display = "coverage", shift.coverage = 0)
    # legend("topleft", col = c("black", "grey"), legend = c("coverage 50% PI", "coverage 95% PI"), lwd = 4, bty = "n")
    
    # coverage:
    plot_from_eval(dat_eval = dat_eval,
                   model = models_to_plot_gm[[model_cat]],
                   location = "GM", target_type = "death",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_to_plot_gm[[model_cat]]], pch = pchs[models_to_plot_gm[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = FALSE, ylim = c(0, 8000))
    title(paste("(b) Weekly incident deaths, Germany,", h, "wk ahead"))
    lines(dat_truth_gm$date, dat_truth_gm$inc_death, type = "o", pch = 15)
    
    # deaths, DE:
    plot_scores(scores = dat_eval_list,
                target = "inc death",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_to_plot_gm[[model_cat]],
                location = "GM",
                start = x_start,
                end = x_end,
                cols = cols[models_to_plot_gm[[model_cat]]], display = "coverage", shift.coverage = 0)
    
    plot_from_eval(dat_eval = dat_eval,
                   model = models_to_plot_pl[[model_cat]],
                   location = "PL", target_type = "case",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_to_plot_pl[[model_cat]]], pch = pchs[models_to_plot_pl[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = "topleft", ylim = c(0, 300000))
    title(paste("(c) Weekly incident cases, Poland,", h, "wk ahead"))
    lines(dat_truth_pl$date, dat_truth_pl$inc_case, type = "o", pch = 15)
    
    # coverage
    plot_scores(scores = dat_eval_list,
                target = "inc case",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_to_plot_pl[[model_cat]],
                location = "PL",
                start = x_start,
                end = x_end,
                cols = cols[models_to_plot_pl[[model_cat]]], display = "coverage", shift.coverage = 0)
    
    # deaths, PL
    plot_from_eval(dat_eval = dat_eval,
                   model = models_to_plot_pl[[model_cat]],
                   location = "PL", target_type = "death",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_to_plot_pl[[model_cat]]], pch = pchs[models_to_plot_pl[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = FALSE, ylim = c(0, 5000))
    title(paste("(d) Weekly incident deaths, Poland,", h, "wk ahead"))
    lines(dat_truth_pl$date, dat_truth_pl$inc_death, type = "o", pch = 15)
    
    # coverage:
    plot_scores(scores = dat_eval_list,
                target = "inc death",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_to_plot_pl[[model_cat]],
                location = "PL",
                start = x_start,
                end = x_end,
                cols = cols[models_to_plot_pl[[model_cat]]], display = "coverage", shift.coverage = 0)
    # legend("topleft", col = c("black", "grey"), legend = c("coverage 50% PI", "coverage 95% PI"), lwd = 4, bty = "n")
    dev.off()
  }
}

