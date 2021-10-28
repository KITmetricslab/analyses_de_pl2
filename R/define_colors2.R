# Defining colours, line and point types for other scripts.

library(pals)

cols_raw <- kelly(12)[-c(1, 2, 9)]

cols_baseline <- c("KIT-baseline" = "darkgrey",
                   "KIT-time_series_baseline" = "darkgrey",
                   "KIT-extrapolation_baseline" = cols_raw[8])

pch_baseline <- c("KIT-baseline" = 21,
                  "KIT-time_series_baseline" = 22, 
                  "KIT-extrapolation_baseline" = 24)

lty_baseline <- c("KIT-baseline" = 1,
                  "KIT-time_series_baseline" = 2, 
                  "KIT-extrapolation_baseline" = 4)

cols_ensemble <- c("KITCOVIDhub-mean_ensemble" = "black",
                   "KITCOVIDhub-median_ensemble" = "black",
                   "KITCOVIDhub-inverse_wis_ensemble" = "black")

pch_ensemble <- c("KITCOVIDhub-mean_ensemble" = 24,
                  "KITCOVIDhub-median_ensemble" = 22,
                  "KITCOVIDhub-inverse_wis_ensemble" = 22)

lty_ensemble <- c("KITCOVIDhub-mean_ensemble" = 2,
                  "KITCOVIDhub-median_ensemble" = 1,
                  "KITCOVIDhub-inverse_wis_ensemble" = 4)

cols_submitted <- c("epiforecasts-EpiExpert" = cols_raw[1],
                    "epiforecasts-EpiNow2" = cols_raw[2],
                    "FIAS_FZJ-Epi1Ger" = cols_raw[3],
                    "SDSC_ISG-TrendModel" = cols_raw[4],
                    "ICM-agentModel" = cols_raw[3],
                    "IHME-CurveFit" = cols_raw[5],
                    "Imperial-ensemble2" = cols_raw[8],
                    "itwm-dSEIR" = cols_raw[9],
                    "ITWW-county_repro" = cols_raw[4],
                    "LANL-GrowthRate" = cols_raw[5],
                    "LeipzigIMISE-SECIR" = cols_raw[6],
                    "MIMUW-StochSEIR" = cols_raw[9],
                    "MIT_CovidAnalytics-DELPHI" = cols_raw[7],
                    "MOCOS-agent1" = cols_raw[6],
                    "UCLA-SuEIR" = cols_raw[8],
                    "USC-SIkJalpha" = cols_raw[9],
                    "Karlen-pypm" = cols_raw[8])

pch_submitted <- c("epiforecasts-EpiExpert" = 21,
                    "epiforecasts-EpiNow2" = 21,
                    "FIAS_FZJ-Epi1Ger" = 23,
                    "SDSC_ISG-TrendModel" = 23,
                    "ICM-agentModel" = 24,
                    "IHME-CurveFit" = 24,
                    "Imperial-ensemble2" = 24,
                    "itwm-dSEIR" = 24,
                    "ITWW-county_repro" = 24,
                    "LANL-GrowthRate" = 22,
                    "LeipzigIMISE-SECIR" = 25,
                    "MIMUW-StochSEIR" = 21,
                    "MIT_CovidAnalytics-DELPHI" = 21,
                    "MOCOS-agent1" = 24,
                    "UCLA-SuEIR" = 21,
                    "USC-SIkJalpha" = 22,
                    "Karlen-pypm" = 23)

lty_submitted <- c("epiforecasts-EpiExpert" = 1,
                   "epiforecasts-EpiNow2" = 2,
                   "FIAS_FZJ-Epi1Ger" = 4,
                   "SDSC_ISG-TrendModel" = 2,
                   "ICM-agentModel" = 1,
                   "IHME-CurveFit" = 2,
                   "Imperial-ensemble2" = 4,
                   "itwm-dSEIR" = 1,
                   "ITWW-county_repro" = 2,
                   "LANL-GrowthRate" = 4,
                   "LeipzigIMISE-SECIR" = 1,
                   "MIMUW-StochSEIR" = 2,
                   "MIT_CovidAnalytics-DELPHI" = 4,
                   "MOCOS-agent1" = 1,
                   "UCLA-SuEIR" = 2,
                   "USC-SIkJalpha" = 4,
                   "Karlen-pypm" = 1)

cols <- c(cols_baseline, cols_submitted, cols_ensemble)
pchs <- c(pch_baseline, pch_submitted, pch_ensemble)
ltys <- c(lty_baseline, lty_submitted, lty_ensemble)
