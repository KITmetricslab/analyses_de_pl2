# Generate plots of forecasts issued at times at which important trend changes occurred

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

dat_truth_gm_inc_case <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Cases_Germany.csv"),
                         colClasses = list(date = "Date"))
dat_truth_gm_inc_death <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Deaths_Germany.csv"),
                                  colClasses = list(date = "Date"))
dat_truth_pl_inc_case <- read.csv(paste0(path_hub, "/data-truth/MZ/truth_MZ-Incident Cases_Poland.csv"),
                                  colClasses = list(date = "Date"))
dat_truth_pl_inc_death <- read.csv(paste0(path_hub, "/data-truth/MZ/truth_MZ-Incident Deaths_Poland.csv"),
                                  colClasses = list(date = "Date"))




### Upward turn in Germany, February/March 2021:
pdf(paste0("../figures/upward_turn_de.pdf"), width = 8, height = 6)
# par(mfrow = c(2, 3), las = 1)

layout(matrix(c(1, 2,
                3, 4,
                5, 5), ncol = 2, byrow = TRUE), heights = c(2, 2, 1))
show_intervals_de <- FALSE

# models to show:
models_to_plot_de <- c("KITCOVIDhub-median_ensemble", "Karlen-pypm", 
                       "ITWW-county_repro", "LeipzigIMISE-SECIR",
                       "FIAS_FZJ-Epi1Ger", "LANL-GrowthRate",
                       "epiforecasts-EpiNow2", "epiforecasts-EpiExpert",
                       "itwm-dSEIR", "USC-SIkJalpha")
models_to_plot_de <- sort(models_to_plot_de)

# horizontal shifts to avoid overplotting:
shifts_de <- seq(from = -1, to = 1, length.out = 10)# rep(0, length(models_to_plot_de)) # c(-2.1, -0.7, 0.7, 2.1)

# generate plot:
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = models_to_plot_de,
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-15",
               width = 1, col = cols[models_to_plot_de], pch = pchs[models_to_plot_de],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = shifts_de,
               legend = FALSE, 
               ylim = c(0, 150000), show_intervals = show_intervals_de)
mtext(side= 3, paste("(a) Incident cases, Germany,\n forecasts from 2021-02-15"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = models_to_plot_de,
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols[models_to_plot_de], pch = pchs[models_to_plot_de],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = shifts_de,
               legend = FALSE, 
               ylim = c(0, 150000), show_intervals = show_intervals_de)
mtext(side= 3, paste("(b) Incident cases, Germany,\n forecasts from 2021-02-22"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = models_to_plot_de,
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-03-01",
               width = 1, col = cols[models_to_plot_de], pch = pchs[models_to_plot_de],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = shifts_de,
               legend = FALSE, 
               ylim = c(0, 150000), show_intervals = show_intervals_de)
# plot title:
mtext(side= 3, paste("(c) Incident cases, Germany,\n forecasts from 2021-03-01"))

# add plot on available variant data:

# available variant data
dates_10 <- as.Date("2021-01-16") + (0:3)*7
values_10 <- c(1/49, 121/3291, 1452/30348, 2642/23530)

dates_17 <- as.Date("2021-01-16") + (0:4)*7
values_17 <- c(1/49, 121/3291, 1452/30348, 2686/26531, 6993/30684)

plot(dates_17, values_17, xlim = c(as.Date("2021-01-21"), as.Date("2021-04-01")),
     ylim = 0:1, xlab = "", ylab = "share B.1.1.7", type  ="b", lty = 3, col = "blue")
lines(dates_10, values_10, lty = 1, type = "b")
mtext(side= 3, paste("(d) Available data on share of B.1.1.7"))
legend("topright", c("2021-02-10", "2021-02-17"), col = c("black", "blue"), lty = c(1, 3), bty = "n")


par(mar = c(0, 1, 0, 1))
plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("center", legend = models_to_plot_de, col = cols[models_to_plot_de],
        pch = pchs[models_to_plot_de], bty = "n", cex = 1, ncol  = 3)

dev.off()

### Plot of forecats by the ITWW model

pdf("../figures/itww.pdf", width = 8, height = 4)

par(mfrow = 1:2)

# left panel: forecasts at different time points:
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-01-25",
               width = 1, col = cols["ITWW-county_repro"],
               pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, 
               legend = FALSE, add_vline = FALSE,
               show_intervals = show_intervals_de)
# plot title
mtext(side= 3, paste("(a) Incident cases, Germany,\n forecasts from the ITWW model\n in different weeks"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-01",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE,
               show_intervals = show_intervals_de,
               draw_truth = TRUE, add_vline = FALSE)

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-08",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE,
               show_intervals = show_intervals_de,
               draw_truth = TRUE, add_vline = FALSE)

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-15",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE,
               show_intervals = show_intervals_de,
               draw_truth = TRUE, add_vline = FALSE)

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE,
               show_intervals = show_intervals_de,
               draw_truth = TRUE, add_vline = FALSE)

# right panel: Bundesland level forecasts
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM06", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols["ITWW-county_repro"],
               pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, 
               legend = FALSE, 
               ylim = c(0, 25000), show_intervals = show_intervals_de)
text(as.Date("2021-02-01"), 9500, "Lower Saxony", cex = 0.7)


plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM07", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE,
               show_intervals = show_intervals_de,
               draw_truth = TRUE)
text(as.Date("2021-02-12"), 17000, "North-Rhine\n Westphalia", cex = 0.7)


plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_case,
               model = "ITWW-county_repro",
               location = "GM15", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols["ITWW-county_repro"], pch = pchs["ITWW-county_repro"],
               start = as.Date("2021-01-21"), end = as.Date("2021-04-01"), 
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = show_intervals_de,
               draw_truth = TRUE)
text(as.Date("2021-01-30"), 1500, "Thuringia", cex = 0.7)

# plot title:
mtext(side= 3, paste("(b) Incident cases, selected German\n states, forecasts from the\n ITWW model, 2021-02-22"))
dev.off()


### Upward turn in Poland, February 2021:
pdf(paste0("../figures/upward_turn_pl.pdf"), width = 9, height = 7)
par(mfrow = c(2, 2), las = 1)
show_intervals_pl <- FALSE
# models to include:
models_to_plot_pl <- c("ITWW-county_repro", "LANL-GrowthRate",
                       "epiforecasts-EpiNow2", "epiforecasts-EpiExpert",
                       "USC-SIkJalpha", "MOCOS-agent1",
                       "ICM-agentModel", "MIMUW-StochSEIR")
models_to_plot_pl <- c(sort(models_to_plot_pl), "KITCOVIDhub-median_ensemble")

# horizontal shifts to avoid overplotting:
shifts_pl <- seq(from = -1, to = 1, length.out = length(models_to_plot_pl))# rep(0, length(models_to_plot_de)) # c(-2.1, -0.7, 0.7, 2.1)

# generate plot:
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-08",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-01-14"), end = as.Date("2021-04-14"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
# plot title:
mtext(side= 3, paste("(a) Incident cases, Poland,\n forecasts from 2021-02-08"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-15",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-01-14"), end = as.Date("2021-04-14"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
mtext(side= 3, paste("(b) Incident cases, Poland,\n forecasts from 2021-02-15"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-01-14"), end = as.Date("2021-04-14"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
mtext(side= 3, paste("(c) Incident cases, Germany,\n forecasts from 2021-02-22"))

plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = models_to_plot_pl, col = cols[models_to_plot_pl],
       pch = pchs[models_to_plot_pl], bty = "n", cex = 1, ncol = 1)

dev.off()


### Peak of third wave in Poland:

pdf(paste0("../figures/peak_pl.pdf"), width = 9, height = 7)
par(mfrow = c(2, 2), las = 1)
show_intervals_pl <- FALSE
models_to_plot_pl <- c("KITCOVIDhub-median_ensemble",
                       "ITWW-county_repro", "LANL-GrowthRate",
                       "epiforecasts-EpiNow2", "epiforecasts-EpiExpert",
                       "USC-SIkJalpha", "MOCOS-agent1",
                       "ICM-agentModel", "MIMUW-StochSEIR")
models_to_plot_pl <- sort(models_to_plot_pl)

shifts_pl <- seq(from = -1, to = 1, length.out = length(models_to_plot_pl))# rep(0, length(models_to_plot_de)) # c(-2.1, -0.7, 0.7, 2.1)


plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-03-22",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-03-01"), end = as.Date("2021-05-01"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
mtext(side = 3, paste("(a) Incident cases, Poland,\n forecasts from 2021-03-22"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-03-29",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-03-01"), end = as.Date("2021-05-01"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
mtext(side = 3, paste("(b) Incident cases, Poland,\n forecasts from 2021-03-29"))

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_case,
               model = models_to_plot_pl,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2021-04-05",
               width = 1, col = cols[models_to_plot_pl], pch = pchs[models_to_plot_pl],
               start = as.Date("2021-03-01"), end = as.Date("2021-05-01"), 
               shifts = shifts_pl,
               legend = FALSE, 
               ylim = c(0, 300000), show_intervals = show_intervals_pl)
mtext(side = 3, paste("(c) Incident cases, Poland,\n forecasts from 2021-04-05"))

plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = models_to_plot_pl, col = cols[models_to_plot_pl],
       pch = pchs[models_to_plot_pl], bty = "n", cex = 1, ncol = 1)

dev.off()


### Turning points in death curves:
# (always requires a separate call to plot_from_eval for each curve)

library(RColorBrewer)
col_gradient <- brewer.pal(5, "Spectral")

pdf(paste0("../figures/turning_points_deaths.pdf"), width = 9, height = 4)
par(mfrow = c(1, 3), las = 1)

# Downward turn in Germany:
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-01-04",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               start = as.Date("2020-12-01"), end = as.Date("2021-02-21"), 
               shifts = 0,
               legend = FALSE, 
               ylim = c(0, 8000), show_intervals = FALSE,
               bg = col_gradient[1], col_vline = col_gradient[1])
mtext(side = 3, paste("(a) Incident deaths, Germany,\n forecasts made between\n 2021-01-04 and 2021-01-25"))

# Upward turn in Germany
plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-01-11",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[2], col_vline = col_gradient[2])

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-01-18",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[3], col_vline = col_gradient[3])

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-01-25",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[4], col_vline = col_gradient[4])

legend("bottom", 
       legend = c("Forecast date:", "2021-01-04", "2021-01-11", "2021-01-18", "2021-01-25"),
       col = "black", pt.bg = c(NA, col_gradient[1:4]), pch = c(NA, rep(22, 4)),
       bg = "white")

# upward turn Germany:

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-03-15",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = FALSE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[1], col_vline = col_gradient[1])
mtext(side = 3, paste("(b) Incident deaths, Germany,\n forecasts made between\n 2021-03-15 and 2021-03-29"))


plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-03-22",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[2], col_vline = col_gradient[2])

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_gm_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "GM", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-03-29",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"],
               pch = pchs["KITCOVIDhub-median_ensemble"],
               shifts = 0, add = TRUE,
               legend = FALSE, show_intervals = FALSE,
               bg = col_gradient[3], col_vline = col_gradient[3])

legend("bottom", 
       legend = c("Forecast date:", "2021-03-15", "2021-03-22", "2021-03-29"),
       col = "black", pt.bg = c(NA, col_gradient[1:3]), pch = c(NA, rep(22, 3)),
       bg = "white")


# Upward turn in Poland:

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "PL", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-02-15",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"], 
               pch = pchs["KITCOVIDhub-median_ensemble"],
               start = as.Date("2021-01-15"), end = as.Date("2021-04-01"), 
               shifts = 0,
               legend = FALSE, 
               ylim = c(0, 4000), show_intervals = FALSE,
               bg = col_gradient[1], col_vline = col_gradient[1])

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "PL", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-02-22",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"], 
               pch = pchs["KITCOVIDhub-median_ensemble"],
               start = as.Date("2021-01-15"), end = as.Date("2021-04-01"), 
               shifts = 0,
               legend = FALSE, 
               add = TRUE,
               ylim = c(0, 4000), show_intervals = FALSE,
               bg = col_gradient[2], col_vline = col_gradient[2])

plot_from_eval(dat_eval = dat_eval,
               dat_truth = dat_truth_pl_inc_death,
               model = "KITCOVIDhub-median_ensemble",
               location = "PL", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2021-03-01",
               width = 1, col = cols["KITCOVIDhub-median_ensemble"], 
               pch = pchs["KITCOVIDhub-median_ensemble"],
               start = as.Date("2021-01-15"), end = as.Date("2021-04-01"), 
               shifts = 0,
               legend = FALSE, 
               add = TRUE,
               ylim = c(0, 4000), show_intervals = FALSE,
               bg = col_gradient[3], col_vline = col_gradient[3])
mtext(side = 3, paste("(c) Incident deaths, Poland,\n forecasts made between\n 2021-02-15 and 2021-03-01"))

legend("bottom", 
       legend = c("Forecast date:", "2021-02-15", "2021-02-22", "2021-03-01"),
       col = "black", pt.bg = c(NA, col_gradient[1:3]), pch = c(NA, rep(22, 3)),
       bg = "white")

dev.off()


