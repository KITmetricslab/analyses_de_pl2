# Some plots showing the time series of cases and deaths in DE + PL

library(here)
library(plotrix)
library(zoo)

setwd("/home/johannes/Documents/Projects/second_period/R")
source("functions_paper2.R")


Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
source(paste0(path_hub, "/code/R/auxiliary_functions.R"))

# read in data:
source("load_daily_truths2.R")

# data on government stringency:
dat_stringency <- read.csv("../additional_data/covid-stringency-index.csv", 
                           colClasses = c("Day" = "Date"))
dat_stringency_de <- subset(dat_stringency, Entity == "Germany")
dat_stringency_pl <- subset(dat_stringency, Entity == "Poland")

# data on vaccination:
dat_vacc1 <- read.csv("../additional_data/share-people-vaccinated-covid.csv",
                      colClasses = c("Day" = "Date"))
dat_vacc1_de <- subset(dat_vacc1, Entity == "Germany")
dat_vacc1_pl <- subset(dat_vacc1, Entity == "Poland")
dat_vacc2 <- read.csv("../additional_data/share-people-fully-vaccinated-covid.csv",
                      colClasses = c("Day" = "Date"))
dat_vacc2_de <- subset(dat_vacc2, Entity == "Germany")
dat_vacc2_pl <- subset(dat_vacc2, Entity == "Poland")

par(las = 1)
options(scipen=5)


# custom function to plot time series:
plot_ts <- function(dat_ecdc, dat_jhu, variable, main = "", col_gm = "black", col_pl = "red", points = FALSE,
                    cex.points = 0.9, highlight1 = NULL, highlight2 = NULL,
                    col_highlight1 = "grey92", col_highlight2 = "grey69", 
                    ylim = NULL, add = FALSE, ...){
  dat_ecdc_gm <- subset(dat_ecdc, location == "GM")
  dat_ecdc_pl <- subset(dat_ecdc, location == "PL")
  dat_jhu_gm <- subset(dat_jhu, location == "GM")
  dat_jhu_pl <- subset(dat_jhu, location == "PL")
  
  if(!add){
    if(is.null(ylim)) ylim <- c(0, 1.2*max(c(dat_ecdc_gm[, variable],
                                             dat_jhu_gm[, variable],
                                             dat_ecdc_pl[, variable],
                                             dat_jhu_pl[, variable]), na.rm = TRUE))
    
    plot(dat_ecdc_gm$date, dat_ecdc_gm[, variable], type = "l", xlab = "", ylab = "", col = "white", ylim = ylim, ...)
    if(!is.null(highlight1)) rect(xleft = highlight1[1], ybottom = 0, xright = highlight1[2], ytop = 10^7, col = col_highlight1, border = NA)
    if(!is.null(highlight2)) rect(xleft = highlight2[1], ybottom = 0, xright = highlight2[2], ytop = 10^7, col = col_highlight2, border = NA)
    
  }
  
  lines(dat_ecdc_gm$date, dat_ecdc_gm[, variable], type = "l", col = col_gm)
  lines(dat_jhu_gm$date, dat_jhu_gm[, variable], type = "l", lty = 2, col = col_gm)
  lines(dat_ecdc_pl$date, dat_ecdc_pl[, variable], type = "l", col = col_pl, cex = 0.7)
  lines(dat_jhu_pl$date, dat_jhu_pl[, variable], type = "l", lty = 2, col = col_pl)

  if(points){
    points(dat_ecdc_gm$date, dat_ecdc_gm[, variable], pch = 15, xlab = "", ylab = "", cex = cex.points, col = col_gm)
    points(dat_jhu_gm$date, dat_jhu_gm[, variable], pch = 17, xlab = "", ylab = "", col = col_gm, cex = cex.points)
    points(dat_ecdc_pl$date, dat_ecdc_pl[, variable], pch = 15, xlab = "", ylab = "", cex = cex.points, col = col_pl)
    points(dat_jhu_pl$date, dat_jhu_pl[, variable], pch = 17, xlab = "", ylab = "", cex = cex.points, col = col_pl)
  }

  mtext(side = 3, main, cex = 0.8, line = 0.3)
}

# helper function to add axes:
add_axes <- function(xticks, xticks_labelled){
  axis(1, at = xticks, labels = rep("", length(xticks)), tck=-0.015)
  axis(1, at = xticks_labelled, labels = xticks_labelled, tck = -0.03)
  axis(2); box()
}
# ticks:
# xticks <- seq(from = as.Date("2020-12-05"), by = 7, length.out = 26)
# xticks_labelled <- c(as.Date("2021-01-09"), as.Date("2021-03-27"))

xticks <- seq(from = as.Date("2020-10-03"), by = 7, length.out = 28)
xticks_labelled <- seq(from = as.Date("2020-10-03"), by = 28, length.out = 7)



# define area to highlight:
highlight1 <- c(as.Date("2020-10-12"), as.Date("2020-12-19"))
highlight2 <- c(as.Date("2021-01-11"), as.Date("2021-04-11"))


pdf("../figures/time_series2.pdf", width = 8.5, height = 7)

# structure plot area
layout(matrix(1:8, ncol = 2), heights = c(1.6, 2.6, 2.6, 2.6))
xl <- c(highlight1[1], highlight2[2]) + c(-38, 14)

# Cases:

# small plot showing also first wave:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot_ts(dat_ecdc, dat_jhu, "inc_case", highlight1 = highlight1, highlight2 = highlight2, points = FALSE,
        main = "(a) Weekly incident cases", cex.points = 0.7, axes = FALSE)
axis(2)
axis(1, at = as.Date(c("2020-07-01", "2021-01-01", "2021-07-01")), 
     labels = c("2020-07-01", "2021-01-01", "2021-07-01"))
box()

# large time series plot:
par(las = 1, mar = c(2.5, 4, 1, 2))
plot_ts(dat_ecdc, dat_jhu, "inc_case", highlight1 = highlight1, highlight2 = highlight2,
        xlim = c(highlight1[1], highlight2[2]) + c(-38, 14), axes = FALSE,
        points = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)

# avoid overplotting with vertical dashed lines
plot_ts(dat_ecdc, dat_jhu, "inc_case", add = TRUE, points = FALSE)

legend("topleft", legend = c("Germany", "Poland", "", "RKI/MZ", "JHU"),
       lty = c(1, 1, NA, 1, 2), col = c("black", "red", NA, "black", "black"), bty = "n")

# Variant data
# from https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/DESH/Bericht_VOC_2021-03-31.pdf?__blob=publicationFile
par(mar = c(2.5, 4, 2, 2), las = 1)

# wks <- c(4, 6, 8, 10, 12)
# dates <- as.Date(c("2021-01-31")) + 7*0:4
# percentages <- c(0.056, 0.22, 0.461, 0.722, 0.881)

wks <- 1:20
percentages_de <- c(2.2, 8.5, 5.4, 10.4, 17.8, 21, 33.1, 44.1,
                 52.8, 64.8, 74.3, 80.3, 87.1, 88.8, 88.7,
                 91, 90.2, 87.1, 88.2, 93.4)/100
dates_de <- as.Date(c("2021-01-10")) + 7*0:19

percentages_pl <- c(0.12781954887218044,
                    0.05563909774436092,
                    0.12631578947368416,
                    0.23007518796992485,
                    0.4646616541353384,
                    0.593984962406015,
                    0.7007518796992482,
                    0.8165413533834587,
                    0.893233082706767,
                    0.930827067669173,
                    0.962406015037594,
                    0.9849624060150377,
                    0.9864661654135338,
                    0.9939849624060152,
                    0.9984962406015038) # https://monitor.crs19.pl/2021-05-05/poland/?lang=en
dates_pl <- as.Date(c("2021-01-10")) + 7*0:(length(percentages_pl) - 1)
plot(NULL, xlim = c(highlight1[1], highlight2[2]) + c(-38, 14), ylim = c(0, 1),  axes = FALSE, ylab = "proportion of B.1.1.7")
rect(xleft = highlight1[1], ybottom = 0, xright = highlight1[2], ytop = 100, col = "grey92", border = NA)
rect(xleft = highlight2[1], ybottom = 0, xright = highlight2[2], ytop = 100, col = "grey69", border = NA)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)
points(dates_de, percentages_de, type = "l")
points(dates_pl, percentages_pl, type = "l", col = "red")
mtext(side = 3, cex = 0.8, line = 0.3, "(c) Proportion of VOC B.1.1.7")

legend("topleft", legend = c("Germany", "Poland"),
       lty = c(1, 1, NA, 1, 2), col = c("black", "red"), bty = "n")

# vaccination progress:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot(dat_stringency_de$Day, dat_stringency_de$stringency_index,
     col = "white", ylim = c(0, 0.3), xlab = "", ylab = "share vaccinated",
     xlim = xl, axes = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)
rect(xleft = highlight1[1], ybottom = 0, xright = highlight1[2], ytop = 100, col = "grey92", border = NA)
rect(xleft = highlight2[1], ybottom = 0, xright = highlight2[2], ytop = 100, col = "grey69", border = NA)
lines(dat_vacc1_de$Day, dat_vacc1_de$people_vaccinated_per_hundred/100, type = "l", lty = 3)
lines(dat_vacc2_de$Day, dat_vacc2_de$people_fully_vaccinated_per_hundred/100, type = "l", lty = 1)
lines(dat_vacc1_pl$Day, dat_vacc1_pl$people_vaccinated_per_hundred/100, type = "l", col = "red", lty = 3)
lines(dat_vacc2_pl$Day, dat_vacc2_pl$people_fully_vaccinated_per_hundred/100, type = "l", lty = 1, col = "red")

mtext(side = 3, cex = 0.8, line = 0.3, "(e) Share of population vaccinated")
legend("topleft", legend = c("Germany", "Poland", "", "at least one dose", "fully vaccinated"),
       lty = c(1, 1, NA,3, 1), col = c("black", "red", NA, "black", "black"), bty = "n")
# Deaths:

# small plot showing also first wave:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot_ts(dat_ecdc, dat_jhu, "inc_death", highlight1 = highlight1, highlight2 = highlight2, points = FALSE,
        main = "(b) Weekly incident deaths", cex.points = 0.7, axes = FALSE)
axis(2)
axis(1, at = as.Date(c("2020-07-01", "2021-01-01", "2021-07-01")), 
     labels = c("2020-07-01", "2021-01-01", "2021-07-01"))
box()

# large time series plot:
par(las = 1, mar = c(2.5, 4, 1, 2))
plot_ts(dat_ecdc, dat_jhu, "inc_death", highlight1 = highlight1, highlight2 = highlight2,
        xlim = c(highlight1[1], highlight2[2]) + c(-38, 14), ylim = c(0, 6300),  axes = FALSE,
        points = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)

plot_ts(dat_ecdc, dat_jhu, "inc_death", add = TRUE, points = FALSE)


# Government stringency:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot(dat_stringency_de$Day, dat_stringency_de$stringency_index,
     col = "white", ylim = c(0, 100), xlab = "", ylab = "Stringency Index", xlim = xl,
     axes = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)
rect(xleft = highlight1[1], ybottom = 0, xright = highlight1[2], ytop = 100, col = "grey92", border = NA)
rect(xleft = highlight2[1], ybottom = 0, xright = highlight2[2], ytop = 100, col = "grey69", border = NA)
lines(dat_stringency_de$Day, dat_stringency_de$stringency_index, type = "l")
lines(dat_stringency_pl$Day, dat_stringency_pl$stringency_index, type = "l", col = "red")
mtext(side = 3, cex = 0.8, line = 0.3, "(d) Goverment stringency (according to OxCGRT)")

dev.off()

