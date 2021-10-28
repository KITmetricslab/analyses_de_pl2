# Some auxiliary functions used in the other scripts.

# function for time series plot based on evaluation data:
plot_from_eval <- function(dat_eval, dat_truth = NULL, models, location,
                           target_type, inc_or_cum, horizon = NULL, forecast_date = NULL,
                           ylim = NULL, start = NULL, end = NULL,
                           col = "steelblue", pch = 21, alpha.col = 0.3, add = FALSE,
                           shifts = c(0, 1, -1, -0.5, -0.5), width = 0.5,
                           separate_intervals = TRUE, legend = "topleft",
                           show_intervals = TRUE, col_vline = "black", add_vline = TRUE,
                           bg_points = rep("white", length(models)),
                           draw_truth = !add, add_stars = TRUE){
  
  if(!is.null(forecast_date)) forecast_date <- as.Date(forecast_date)
  
  # restrict to relevant rows:
  dat_eval <- dat_eval[  grepl(target_type, dat_eval$target) &
                           grepl(inc_or_cum, dat_eval$target) &
                           dat_eval$location == location &
                           !grepl("0", dat_eval$target) &
                           !grepl("-1", dat_eval$target), ]
  
  # extract data to plot truth:
  all_dates <- dat_eval$target_end_date[!duplicated(dat_eval$target_end_date)]
  all_truths <- dat_eval$truth[!duplicated(dat_eval$target_end_date)]
  if(!is.null(forecast_date)){
    last_date <- all_dates[(all_dates > forecast_date - 7) & (forecast_date > all_dates)]
    last_truth <- all_truths[all_dates == last_date]
  }else{
    last_date <- last_truth <- NULL
  }
  # if separate truth data provided: compute 7-day incidences
  if(!is.null(dat_truth)){
    all_truths_daily <- dat_truth[dat_truth$location == location,
                                  c("date", "value")]
    all_truths_daily$value7 <- NA
    for(i in 7:nrow(all_truths_daily)) all_truths_daily$value7[i] <- sum(all_truths_daily$value[i - (0:6)])
  }
  
  
  # restrict to models and forecast date or horizon:
  dat_eval <- dat_eval[dat_eval$model %in% models, ]
  if(!is.null(forecast_date)) dat_eval <- dat_eval[dat_eval$timezero == forecast_date, ]
  if(!is.null(horizon)){
    dat_eval <- dat_eval[grepl(horizon, dat_eval$target), ]
  }
  
  # catch in case of only missings:
  if(nrow(dat_eval) == 0 & !add){
    plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
    text(0.5, 0.5, labels = "No forecasts available.")
    return(invisible(list(ylim = NULL)))
  }else{
    
    # choose start, end and ylim if not specified:
    if(is.null(start)) start <- ifelse(is.null(horizon), min(dat_eval$forecast_date) - 35, min(dat_eval$forecast_date) - 21)
    if(is.null(end)) end <- ifelse(is.null(horizon), max(dat_eval$forecast_date) + 63, max(dat_eval$forecast_date) + 35)
    if(is.null(ylim)) ylim <- c(ifelse(inc_or_cum == "inc",
                                       0,
                                       0.75*min(c(dat_eval$value.0.025,
                                                  dat_eval$value.point,
                                                  dat_eval$truth), na.rm = TRUE)),
                                max(c(dat_eval$value.0.975,
                                      dat_eval$value.point,
                                      dat_eval$truth), na.rm = TRUE))
    
    # initialize plot if necessary:
    if(!add){
      plot(dat_eval$target_end_date, dat_eval$truth, ylim = ylim, xlim = c(start, end),
           xlab = "time", ylab = "", # paste(inc_or_cum, target_type), 
           col  ="white")
      # horizontal ablines:
      abline(h = axTicks(2), col = "grey")
    }
    # create transparent color:
    col_transp <- modify_alpha(col, alpha.col)
    
    for(i in seq_along(models)){
      dat_eval_m <- dat_eval[dat_eval$model == models[i], ]
      # add forecasts:
      if(show_intervals){
        plot_weekly_bands(dates = c(last_date, dat_eval_m$target_end_date), 
                          lower = c(last_truth, dat_eval_m$value.0.025),
                          upper = c(last_truth, dat_eval_m$value.0.975),
                          separate_all = separate_intervals,
                          col = lighten(col[i], 0.5), border = NA, width = width, shift = shifts[i])
        plot_weekly_bands(dates = c(last_date, dat_eval_m$target_end_date),
                          lower = c(last_truth, dat_eval_m$value.0.25),
                          upper = c(last_truth, dat_eval_m$value.0.75),
                          separate_all = separate_intervals,
                          col = lighten(col[i], 0), border = NA, width = width, shift = shifts[i])
        
        # add stars where intervals exceed ylim
        if(add_stars){
          upper_exceeds <- dat_eval_m$value.0.975 > ylim[2]
          if(any(upper_exceeds, na.rm = TRUE)){
            points(dat_eval_m$target_end_date[upper_exceeds] + shifts[i], rep(0.98*ylim[2], sum(upper_exceeds)),
                   pch = "*")
          }
          
        }
      }
      if(!is.null(forecast_date)) lines(c(last_date, dat_eval_m$target_end_date + shifts[i]), 
                                        c(last_truth, dat_eval_m$value.point), 
                                        col = col[i], lty = "dotted")
      points(dat_eval_m$target_end_date + shifts[i], dat_eval_m$value.point, pch = pch[i],
             col = col[i], bg = bg_points[i])
    }
    
    if(draw_truth){
      points(all_dates, all_truths, pch = 15, cex = 1)
      if(is.null(dat_truth)){
        lines(all_dates[order(all_dates)], all_truths[order(all_dates)])
      }else{
        lines(all_truths_daily$date, all_truths_daily$value7)
      }
    }
    
    # mark forecast date if necessary:
    if(!is.null(forecast_date) & add_vline) abline(v = forecast_date, lty = 2, col = col_vline)
    
    if(legend != FALSE) legend(legend, col = col, legend = models, pch = pch, bty = "n", cex = 0.8)
    
    # if(!add){
    #   title(paste(horizon, inc_or_cum, target_type, "-", location, "-", model,
    #               ifelse(!is.null(forecast_date), "- Forecast from", ""), forecast_date))
    # }
    
    # return ylim so it can be used in second plot:
    return(invisible(list(ylim = ylim)))
  }
}

letter_in_circle <- function(x, y, letter, col = "black", col_line = "white", cex = 0.8){
  abline(v = x, lty = "dashed", col = col_line)
  draw.circle(x = x, y = y, radius = strwidth("aa", cex = cex)/1.7, border = col, col = "white")
  text(x, y, letter, col = col, cex = cex)
}


# function to create summary table for horizons 1 through 4:
# Arguments:
# dat_eval: the data.frame containing the evaluation results
# target_type: "case" or "death"
# inc_or_cum: "inc" or "cum"
# locations: the locations for which to perform the analysis
# first_forecast_date: first forecast date to include
# last_forecast_date: last forecast date to include
generate_summary <- function(dat_eval, target_type, inc_or_cum, locations, first_forecast_date, last_forecast_date){
  # restrict to relevant entries:
  tab <- dat_eval[dat_eval$timezero >= first_forecast_date &
                    dat_eval$timezero <= last_forecast_date &
                    grepl(paste(inc_or_cum, target_type), dat_eval$target) &
                    !grepl("0 wk", dat_eval$target) &
                    !grepl("-1 wk", dat_eval$target) &
                    dat_eval$location %in% locations, ]
  
  # compute coverages:
  tab$coverage0.5 <- (tab$truth >= tab$value.0.25 &
                        tab$truth <= tab$value.0.75)
  tab$coverage0.95 <- (tab$truth >= tab$value.0.025 &
                         tab$truth <= tab$value.0.975)
  # extra variable needed for re-formatting:
  tab$horizon <- substr(tab$target, start = 1, stop = 1)
  
  # select relevant columns:
  tab <- tab[, c("location", "timezero", "model", "ae", "wis", "wgt_iw", "wgt_pen_u", "wgt_pen_l",
                 "coverage0.5", "coverage0.95", "horizon")]
  
  # bring to wide format with separate variables for horizons:
  tab_wide <- reshape(tab, direction = "wide", timevar = "horizon",
                      idvar = c("timezero", "model", "location"))
  
  
  # some vectors to handle column names:
  cols_imputation <- c("ae.1", "ae.2", "ae.3", "ae.4",
                       "wis.1", "wis.2", "wis.3", "wis.4")
  cols_numbers <- c(cols_imputation,
                    paste0("coverage0.5.", 1:4),
                    paste0("coverage0.95.", 1:4),
                    paste0("wgt_iw.", 1:4),
                    paste0("wgt_pen_u.", 1:4),
                    paste0("wgt_pen_l.", 1:4))
  
  # identify NA values:
  tab_wide_is_available <- tab_wide; tab_wide_is_available[, cols_numbers] <- !is.na(tab_wide_is_available[, cols_numbers])
  tab_wide_is_available$timezero <- tab_wide_is_available$location <- NULL
  
  # count number of available scores per model:
  available_per_model <- aggregate(. ~ model, data = tab_wide_is_available, FUN = sum)
  # what is the possible maximum?
  available_per_model_max <- apply(available_per_model[, cols_numbers], MARGIN = 2, FUN = max)
  available_per_model_rel <- available_per_model
  # compute relative share of covered targets/dates:
  available_per_model_rel[, cols_numbers] <- available_per_model[, cols_numbers]/
    matrix(available_per_model_max, ncol = length(available_per_model_max), nrow = nrow(available_per_model), byrow = TRUE)
  
  # generate version where worst scores are filled in where NA
  timezeros <- sort(unique(tab_wide$timezero))
  replace_na <- function(vect){
    if(all(is.na(vect))) return(NA)
    vect[is.na(vect)] <- max(vect, na.rm = TRUE)
    return(vect)
  }
  
  tab_wide_imputed <- tab_wide
  for(tz in timezeros){
    for(loc in locations){
      inds <- which(tab_wide$timezero == tz & tab_wide$location == loc)
      for(co in cols_imputation){
        tab_wide_imputed[inds, co] <- replace_na(tab_wide[inds, co])
      }
    }
  }
  
  # aggregate including NA values ("raw"):
  tab_wide$location <- NULL
  tab_wide$timezero <- NULL
  summary_tab_raw <- aggregate(. ~ model, data = tab_wide,
                               FUN = mean, na.rm = TRUE, na.action = na.pass)
  
  # aggregate with imputation:
  tab_wide_imputed$location <- NULL
  tab_wide_imputed$timezero <- NULL
  summary_tab_imputed <- aggregate(. ~ model, data = tab_wide_imputed,
                                   FUN = mean, na.rm = TRUE, na.action = na.pass)
  
  # aggregate available_per_model
  available_per_model$location <- NULL
  available_per_model$timezero <- NULL
  summary_available_per_model <- aggregate(. ~ model, data = available_per_model,
                                           FUN = sum, na.rm = TRUE, na.action = na.pass)
  
  # aggregate available_per_model_rel
  available_per_model_rel$location <- NULL
  available_per_model_rel$timezero <- NULL
  summary_available_per_model_rel <- aggregate(. ~ model, data = available_per_model_rel,
                                               FUN = mean, na.rm = TRUE, na.action = na.pass)
  
  # set to NA where not enough observations available
  for(co in cols_imputation){
    summary_tab_raw[which(summary_available_per_model_rel[, co] < 1), co] <- NA
    summary_tab_imputed[which(summary_available_per_model_rel[, co] < 2/3), co] <- NA
  }
  # and remove NaN:
  for(co in cols_numbers){
    summary_tab_raw[is.nan(summary_tab_raw[, co]), co] <- NA
    summary_tab_imputed[is.nan(summary_tab_imputed[, co]), co] <- NA
  }
  
  return(list(summary_tab_raw = summary_tab_raw,
              summary_tab_imputed = summary_tab_imputed,
              summary_available_per_model = summary_available_per_model,
              summary_available_per_model_rel = summary_available_per_model_rel,
              summary_available_per_model_max = available_per_model_max))
}

# helper function to restrict summary to some horizons:
restrict_summary <- function(summ, horizons = 1:2){
  columns_to_keep <- sapply(horizons, function(x) paste0(c("ae.", "wis.", "coverage0.5.", "coverage0.95."), x))
  
  for(el in c("summary_tab_raw", "summary_tab_imputed", "available_per_model")){
    summ[[el]] <- summ[[el]][, c("model", columns_to_keep)]
  }
  summ$summary_available_per_model_max <- summ$summary_available_per_model_max[columns_to_keep]
  return(summ)
}

# helper function to merge summaries of incident and cumulative results:
merge_inc_cum_summaries <- function(summ_inc, summ_cum){
  summ <- list()
  for(el in c("summary_tab_raw", "summary_tab_imputed", "summary_available_per_model")){
    summ[[el]] <- merge(summ_inc[[el]], summ_cum[[el]], by = "model", all.x = TRUE, all.y = TRUE,
                        suffixes = c(".inc", ".cum"))
  }
  
  names(summ_inc$summary_available_per_model_max) <- paste0(names(summ_inc$summary_available_per_model_max), ".inc")
  names(summ_cum$summary_available_per_model_max) <- paste0(names(summ_cum$summary_available_per_model_max), ".cum")
  summ$summary_available_per_model_max <- c(summ_inc$summary_available_per_model_max,
                                            summ_cum$summary_available_per_model_max)
  
  return(summ)
}

# helper function to merge summaries of incident and cumulative results:
merge_case_death_summaries <- function(summ_case, summ_death){
  summ <- list()
  for(el in c("summary_tab_raw", "summary_tab_imputed", "summary_available_per_model")){
    summ[[el]] <- merge(summ_case[[el]], summ_death[[el]], by = "model", all.x = TRUE, all.y = TRUE,
                        suffixes = c(".case", ".death"))
  }
  
  names(summ_case$summary_available_per_model_max) <- paste0(names(summ_case$summary_available_per_model_max), ".case")
  names(summ_death$summary_available_per_model_max) <- paste0(names(summ_death$summary_available_per_model_max), ".death")
  summ$summary_available_per_model_max <- c(summ_case$summary_available_per_model_max,
                                            summ_death$summary_available_per_model_max)
  
  return(summ)
}

# function for printing:
xtable_summary_tab <- function(summary_tab){
  tab <- summary_tab$summary_tab_imputed
  
  # re-format proportions:
  columns_coverage <- colnames(tab)[grepl("coverage", colnames(tab))]
  for(co in columns_coverage){
    inds <- which(!is.na(tab[, co]))
    tab[inds, co] <- round(tab[inds, co]*summary_tab$summary_available_per_model[inds, co])
    tab[inds, co] <- paste0(tab[inds, co], "/", summary_tab$summary_available_per_model[inds, co])
  }
  
  # add stars:
  columns_scores <- colnames(tab)[grepl("ae", colnames(tab)) | grepl("wis", colnames(tab))]
  
  for(co in columns_scores){
    inds <- which(summary_tab$summary_available_per_model[, co] < summary_tab$summary_available_per_model_max[co] &
                    !is.na(tab[, co]))
    tab[, co] <- format(tab[, co], digits = 0, scientific = FALSE, big.mark = ",")
    tab[grepl("NA", tab[, co]), co] <- "" # remove NA
    tab[inds, co] <- paste0("*", tab[inds, co])
  }
  
  tab <- tab[order(tab$model), ]
  
  tab_baselines <- tab[grepl("baseline", tab$model), ]
  tab_baselines <- tab_baselines[order(tab_baselines$model), ]
  
  tab_ensembles <- tab[grepl("KITCOVIDhub", tab$model), ]
  tab_ensembles <- tab_ensembles[order(tab_ensembles$model), ]
  
  tab_members <- tab[!grepl("KITCOVIDhub", tab$model) & !grepl("baseline", tab$model), ]
  tab_members <- tab_members[order(tab_members$model), ]
  
  tab_to_print <- rbind(tab_members, tab_baselines, tab_ensembles)
  
  hline.after <- if(nrow(tab_members) != 0) nrow(tab_members) + c(0, nrow(tab_baselines)) else NULL
  
  print(xtable(tab_to_print), hline.after = hline.after,
        include.rownames=FALSE, only.contents = TRUE, include.colnames = FALSE,
        format.args = list(big.mark = ","))
}


# plot function to show decay of performance over horizons
# Arguments:
# summ: the summary of model scores, as generated above
# models: the models to include in the comparison
# imputed: should the imputed scores be used?
# col: colors
# legend: should a legend be added?
# max_horizon_ up to which forecast horizon should results be shown?
# show_ae: should absolute errors be shown as diamonds?
# add_baseline: should a grey area be added for the baseline model
# main: plot title
plot_performance_decay <- function(summ, models, imputed = TRUE, col, lty, legend = FALSE, 
                                   max_horizon = 4, show_ae = TRUE, add_baseline = TRUE, main = ""){
  if(imputed){
    tab <- summ$summary_tab_imputed
  }else{
    tab <- summ$summary_tab_raw
  }
  
  yl <- c(0, max(c(tab$wis.1, tab[, paste0("wis.", max_horizon)]), na.rm = TRUE))
  
  plot(NULL, xlim = c(0.5, 4.5), ylim = yl,
       xlab = "", ylab = "mean WIS or AE", axes = FALSE)
  axis(3, at = 1:max_horizon); axis(2); box()
  mtext("horizon", line = 1)
  mtext(main, line = 2.2, cex = 1.2)
  
  if(add_baseline){
    x <- rep((0:max_horizon) + 0.5, each = 2)
    wis_baseline <- unlist(subset(tab, model == "KIT-baseline")[, paste0("wis.", 1:max_horizon)])
    polygon(x,
            c(yl[2], rep(wis_baseline, each = 2), yl[2]), 
            col = "lightgrey", border = NA)
    ae_baseline <- unlist(subset(tab, model == "KIT-baseline")[, paste0("ae.", 1:max_horizon)])
    lines(x[-c(1, length(x))], rep(ae_baseline, each = 2), col = "darkgrey")
  }
  abline(v = 1:max_horizon - 0.5, lty = "dotted")
  
  n_models <- length(models)
  
  par(xpd = TRUE)
  for(m in seq_along(models)){
    pen_l <- unlist(subset(tab, model == models[m])[, paste0("wgt_pen_l.", 1:max_horizon)])
    pen_u <- unlist(subset(tab, model == models[m])[, paste0("wgt_pen_u.", 1:max_horizon)])
    iw <- unlist(subset(tab, model == models[m])[, paste0("wgt_iw.", 1:max_horizon)])
    ae <- unlist(subset(tab, model == models[m])[, paste0("ae.", 1:max_horizon)])
    
    for(i in 1:4){
      x <-  i - 0.5 + m/(n_models + 1)
      add_score_decomp(x = x, pen_l = pen_l[i], pen_u = pen_u[i], 
                       iw = iw[i], col = col[m], width = 1/(n_models + 1))
      points(x, ae[i], pch = 23, col = col[m], bg = "white")
      # mtext(models[m], at = x, side = 1, cex = 0.5, las = 2, line = 0.3)
      text(x, models[m], srt = 45, y = -yl[2]/20, adj = 1, cex = 0.8)
    }
  }
  par(xpd = FALSE)
  
  if(legend) legend("topleft", legend = c(if(show_ae) "absolute error",
                                          "penalties for overprediction",
                                          "spread of forecasts",
                                          "penalties for underprediction",
                                          if(add_baseline) "mean WIS and AE of KIT-baseline"), 
                    pt.bg = c(if(show_ae) "white",
                              lighten_colour(col[1], 0.3),
                              col[1],
                              "white",
                              if(add_baseline) "lightgrey"),
                    col = c(if(show_ae) col[1], 
                            rep(col[1], 3),
                            if(add_baseline) "lightgrey"),
                    pch = c(if(show_ae) 23, 22, 22, 22, 
                            if(add_baseline) 22,
                            if(add_baseline) NA),
                    lty = c(if(show_ae) NA, NA, NA, NA, 
                            if(add_baseline) 1),
                    cex = 0.9, bg = "white")
}


# Helper function to show WIS decomposition using little stacked barplots.
# Arguments:
# x: the x values, typically dates
# pen_l: penalties for overprediction
# pen_u: penalties for underprediction
# iw: average interval widths
# col: color
# alpha.col: alpha value for lighter/transparent colur used in upper "bin"
# width: the width of the bar
add_score_decomp <- function(x, pen_l, pen_u, iw, col, alpha.col = 0.3, width = 0.8){
  # generate transparent color
  col_transp <- lighten_colour(col, alpha.col)
  if(!is.na(pen_u)){ # plot only when pen_u is available. Otherwise wi can get plotted even though the others are NA
    # bottom bar: penalty for overprediction
    if(pen_l > 0) rect(xleft = x - width/2, ybottom = 0, xright = x + width/2, ytop = pen_l,
                       col = "white", border = col)
    # middle bar: width of prediction intervals
    rect(xleft = x - width/2, ybottom = pen_l, xright = x + width/2, ytop = pen_l + iw,
         col = col, border = col)
    # top bar: penalty for underprediction
    if(pen_u > 0){
      rect(xleft = x - width/2, ybottom = pen_l + iw, xright = x + width/2, ytop = pen_l + iw + pen_u,
           col = col_transp, border = col)
    }
  }
}

# modify the alpha value of a given color (to generate transparent versions for prediction bands)
lighten_colour <- function(col, alpha){
  x <- col2rgb(col)/255
  if(all(x < 0.1)) x <- x + 0.1
  rgb(x[1]^alpha, x[2]^alpha, x[3]^alpha)
}