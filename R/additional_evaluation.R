# Run evaluation for additional forecasts from FIAS_FZJ-Epi1Ger and IHME-CurveFit
# these are not contained in the main evaluation files as they were added retrospectively (FIAS)
# or required specific alignment with the forecast dates (IHME)

# setwd("/home/johannes/Documents/Projects/second_period/R")

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
source(paste0(path_hub, "/code/R/evaluation_functions.R"))
source(paste0(path_hub, "/code/R/auxiliary_functions.R"))



# get truth data:
dat_truth <- list()
dat_truth$JHU <- truth_to_long(read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_jhu.csv"),
                                        colClasses = list("date" = "Date")))
dat_truth$ECDC <- truth_to_long(read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_ecdc.csv"),
                                         colClasses = list("date" = "Date")))

# get data on truth data use:
truth_data_use <- read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_data_use_detailed.csv"),
                           stringsAsFactors = FALSE, colClasses = list("starting_from"  ="Date"))

# data.frame to store results:
tags_fips <- c("", paste0("0", 1:9), 10:16)
truth_data_use <- data.frame(model = c(rep("FIAS_FZJ-Epi1Ger", 17),
                                       rep("IHME-CurveFit", 34)),
                             location = c(paste0("GM", tags_fips),
                                          paste0("GM", tags_fips),
                                          paste0("PL", tags_fips)),
                             truth_data_source = c(rep("ECDC", 17),
                                                   rep("ECDC", 17),
                                                   rep("JHU", 17)),
                             starting_from = as.Date("2020-01-01"))

# get names of models:
models <- unique(truth_data_use$model)

# perform evaluation for different truth data sources:
for(truth_eval in c("ECDC", "JHU")){
  evaluations <- NULL
  
  for(model in models){
    cat("Starting", model, " / ", truth_eval, "\n")
    path <- paste0("../additional_forecasts/", model)
    files <- list.files(path)
    files <- files[grepl(".csv", files) & grepl("20", files)]
    
    # evaluate death forecasts:
    files_death <- files[!grepl("case", files) & !grepl("ICU", files)]
    forecast_dates_death <- get_date_from_filename(files_death)
    files_death <- files_death[forecast_dates_death %in% choose_relevant_dates(forecast_dates_death)]
    
    for(i in seq_along(files_death)){
      # read in forecasts:
      forecasts <- read_week_ahead(paste0(path, "/", files_death[i]))
      forecasts$X <- NULL # remove row numbers if necessary
      
      # evaluate:
      if(nrow(forecasts) > 0){
        eval_temp <- evaluate_forecasts(forecasts,
                                        name_truth_eval = truth_eval,
                                        dat_truth = dat_truth,
                                        truth_data_use = truth_data_use)
        eval_temp <- cbind(model = model,
                           timezero = next_monday(get_date_from_filename(files_death[i])),
                           eval_temp)
        
        if(is.null(eval)){
          evaluations <- eval_temp
        }else{
          evaluations <- rbind(evaluations, eval_temp)
        }
      }
    }
    
    # evaluate case forecasts:
    files_case <- files[grepl("case", files)]
    forecast_dates_case <- get_date_from_filename(files_case)
    files_case <- files_case[forecast_dates_case %in% choose_relevant_dates(forecast_dates_case)]
    
    for(i in seq_along(files_case)){
      # read in forecasts:
      forecasts <- read_week_ahead(paste0(path, "/", files_case[i]))
      forecasts$X <- NULL # remove row numbers if necessary
      
      # evaluate:
      if(nrow(forecasts) > 0){
        eval_temp <- evaluate_forecasts(forecasts,
                                        name_truth_eval = truth_eval,
                                        dat_truth = dat_truth, 
                                        truth_data_use = truth_data_use)
        eval_temp <- cbind(model = model,
                           timezero = next_monday(get_date_from_filename(files_case[i])),
                           eval_temp)
        
        if(is.null(eval)){
          evaluations <- eval_temp
        }else{
          evaluations <- rbind(evaluations, eval_temp)
        }
      }
    }
  }
  
  # write out:
  cat("Writing out", truth_eval, "\n")
  write.csv(evaluations, file = paste0("../additional_forecasts/additional_evaluation-", truth_eval, ".csv"), row.names = FALSE)
}
