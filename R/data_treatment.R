#' Function for filtering seasonalities based on XXX
#' 
#' @param input_frame A data frame containing the times series data.
#' @return data_filt Same dataframe with deseasonalised price
#' 
#' @export
#' 
long_run_trend_season <- function(input_frame) {  
  data_frame <- input_frame %>%
    transmute(price,
              trend = 1:n(),
              ewma = ewma(price))
  
  # Run linear and non-linear combinations to obtain starting values.
  data_frame_init <- data_frame %>%
    transmute(price,
              trend = sin(2 * pi * (trend / 365 + 1)),
              ewma)
  out_init <- lm(price ~ trend + ewma,
                 data = data_frame_init) %>%
    tidy %>%
    use_series(estimate)    
  
  out_init_1 <- nls(price ~ (out_init[2] * sin(2 * pi * (trend / 365 + beta_2)) +
                               out_init[1] + out_init[3] * ewma),
                    data = data_frame,
                    start = list(
                      beta_2 = 1),
                    trace = TRUE,
                    control = list(
                      maxiter = 5000)) %>%
    tidy %>%
    use_series(estimate)
  
  data_frame_init_1 <- data_frame %>%
    transmute(price,
              trend = sin(2 * pi * (trend / 365 + out_init_1[1])),
              ewma = ewma(price))
  out_init_2 <- lm(price ~ trend + ewma,
                   data = data_frame_init_1) %>%
    tidy %>%
    use_series(estimate)
  
  # Run model
  trend_seas_fit <- nls(price ~ (beta_1 * sin(2 * pi * (trend / 365 + beta_2)) +
                                   beta_3 + beta_4 * ewma),
                        data = data_frame,
                        start = list(
                          beta_1 = out_init_2[2],
                          beta_2 = out_init_1[1],
                          beta_3 = out_init_2[1],
                          beta_4 = out_init_2[3]),
                        trace = TRUE,
                        algorithm = "default", # GN. Same convergence for "port"
                        control = list(
                          maxiter = 100,
                          tol = 1e-06,
                          printEval = TRUE)) %>%
    tidy %>%
    use_series(estimate)  
}

#' Function calculating short run season
#' @param input_frame A data frame with a trend and a price
#' @export
#' 
short_run_season <- function(input_frame) {
  data_frame <- input_frame %>%
    transmute(price,
              week_day = date %>% as.Date %>% format("%a") %>% as.factor)
  
  data_frame <- cbind(data_frame,
                      data.frame(model.matrix(~ week_day - 1,
                                              data = data_frame))) %>%
    select(-week_day, -week_dayMon)
  
  short_seas_fit <- lm(price ~ .,
                       data = data_frame)
  
  return(short_seas_fit)
}

#' Function replacing missing values (NA) using linear interpolation between
#' closest finite observations. If start/end point is missing, they are
#' replaced by a mean of the 2 the closets observations possible.
#'
#' @param input_data An atomic vector containing data to be fixed.
#' @return An atomic vector filtered for missing values.
#' @export
na_filter <- function(input_data) {
  cat("Replacing missing values\n")
  
  .idx <- which(is.na(input_data) == TRUE)
  
  if (length(.idx) > 0) {
    for (ii in 1:length(.idx)) {
      for (jj in 1:(length(input_data) - .idx[ii])) {
        if (!is.na(input_data[.idx[ii] + jj]) == TRUE) {
          .bp <- jj
          break
        }
      }
      if (length(input_data[.idx[ii] - 1]) == 0) {
        input_data[.idx[ii]] <- mean(input_data[(.idx[ii] + .bp):(.idx[ii] +
                                                                    .bp + 1)])
      } else if (!exists(".bp")) {
        input_data[.idx[ii]] <- mean(input_data[(.idx[ii] - 2):(.idx[ii] - 1)])
        cat(paste0("Replaced ", ii, "\n"))
      } else {
        input_data[.idx[ii]] <- input_data[.idx[ii] - 1] +
          (input_data[.idx[ii] + .bp] - input_data[.idx[ii] - 1]) / (.bp + 1)
        rm(.bp)
        cat(paste0("Replaced ", ii, "\n"))
      }
    }
  }
  cat("... Done\n")
  return(input_data)
}

#' Outlier filter function
#' 
#' @param data_input A data frame containing a price to be outlier filtered.
#' @param std_filt Number of standard deviations to filter on.
#' @export
outlier_filt <- function(input_frame, std_filt = 4)
{
  cat("Filtering Outliers\n")
  cat(paste0("Outliers outside of ", std_filt, " being replaced by NA"))
  data_transform <- input_frame
  data_transform$price[which(data_transform$price >
                               std_filt * sd(data_transform$price) |
                               data_transform$price <
                               - std_filt * sd(data_transform$price))
                       ] <- NA
  cat(" ... Done")
  output_frame <- data_transform %>% 
    mutate(price = na_filter(price))
  
  cat(" ... Outliers filtered")
  return(output_frame)
}

#' @export
seasonal_filter <- function(input_frame) {
  data_frame <- input_frame %>%
    transmute(price,
              trend = 1:n(),
              dum_week = ifelse(format(date, "%a") == "Sat" | 
                                  format(date, "%a") == "Sun", 0, 1))
  
  out <- nls(price ~ (k0 + k1 * trend + k2  * sin((trend + k3) * 2 * pi / 365) + 
               k4 * sin((trend + k5) * 4 * pi / 365) + k6 * dum_week),
             data = data_frame,
             start = list(
               k0 = 0.1,
               k1 = 0.1,
               k2 = 0.1,
               k3 = 0.1,
               k4 = 0.1,
               k5 = 0.1,
               k6 = 0.1),
             trace = TRUE,
             control = list(
               maxiter = 5000,
               tol = 1e-07,
               printEval = TRUE)) %>%
    tidy %>%
    use_series(estimate)
}

#' @export
pre_model_processing <- function(input_frame) {
  set_date_time()
  
  data_frame <- input_frame %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  trend_seas_fit <- seasonal_filter(data_frame)
  deseason_data_frame <- data_frame %>% 
    transmute(date,
              trend = 1:n(),
              dum_week = ifelse(format(date, "%a") == "Sat" | 
                                  format(date, "%a") == "Sun", 0, 1),
              price = price -
                (trend_seas_fit[1] + trend_seas_fit[2] * trend +
                   trend_seas_fit[3]  * sin((trend + trend_seas_fit[4])
                                            * 2 * pi / 365) +
                   trend_seas_fit[5] * sin((trend + trend_seas_fit[6])
                                           * 4 * pi / 365) +
                   trend_seas_fit[7] * dum_week),
              trend_seas = (trend_seas_fit[1] + trend_seas_fit[2] * trend +
                              trend_seas_fit[3]  * sin((trend + trend_seas_fit[4])
                                                       * 2 * pi / 365) +
                              trend_seas_fit[5] * sin((trend + trend_seas_fit[6])
                                                      * 4 * pi / 365) +
                              trend_seas_fit[7] * dum_week)) %>%
    select(-trend)
  
  deseason_filtered_frame <- outlier_filt(deseason_data_frame, 3)
}

#' @export
filter_only_outliers <- function(input_frame, std_filt = 3) {
  set_date_time()
  
  data_frame <- input_frame %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  trend_seas_fit <- seasonal_filter(data_frame)
  deseason_data_frame <- data_frame %>% 
    transmute(date,
              trend = 1:n(),
              dum_week = ifelse(format(date, "%a") == "Sat" | 
                                  format(date, "%a") == "Sun", 0, 1),
              price = price -
                (trend_seas_fit[1] + trend_seas_fit[2] * trend +
                   trend_seas_fit[3]  * sin((trend + trend_seas_fit[4])
                                            * 2 * pi / 365) +
                   trend_seas_fit[5] * sin((trend + trend_seas_fit[6])
                                           * 4 * pi / 365) +
                   trend_seas_fit[7] * dum_week),
              trend_seas = (trend_seas_fit[1] + trend_seas_fit[2] * trend +
                              trend_seas_fit[3]  * sin((trend + trend_seas_fit[4])
                                                       * 2 * pi / 365) +
                              trend_seas_fit[5] * sin((trend + trend_seas_fit[6])
                                                      * 4 * pi / 365) +
                              trend_seas_fit[7] * dum_week)) %>%
    select(-trend)
  
  data_frame$price[which(deseason_data_frame$price >
                      std_filt * sd(deseason_data_frame$price) |
                        deseason_data_frame$price <
                      - std_filt * sd(deseason_data_frame$price))] <- NA
  
  data_frame_out <- data_frame %>%
    mutate(price = na_filter(price))
  
  return(data_frame_out)
}