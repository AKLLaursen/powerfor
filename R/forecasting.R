#' @export
oos_forecast_daily <- function(input_frame, input_frame_exp, test_start = "2013-01-01", h = 1,
                               cores = 8L, forecast_type = "ar",
                               market = "spot", country = "de") {
  test_start %<>% as.Date
  
  input_frame_exp %<>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load),
           diff_residual_load = residual_load - lag(residual_load, 1))
  
  input_frame %<>% mutate(date = date %>% as.Date) %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup %>%
    left_join(input_frame_exp, by = c("date"))
  
  date_vec <- seq(test_start, tail(input_frame$date, 1), by = "day")
  
  cl <- makeCluster(getOption("cl.cores", cores), type = "PSOCK")
  clusterExport(cl,
                varlist = c("input_frame",
                            "date_vec"),
                envir = environment())
  clusterEvalQ(cl,
               {
                 library(magrittr)
                 library(dplyr)
                 library(e1071)
                 library(rugarch)
                 library(powerfor)
               })
  cat(paste("\nSent to clusters at:", Sys.time()))
  
  fore_out <- parLapply(cl, date_vec, function(x) {
    
    file_conn <- file("C:/git/r/powerfor/inst/forecasts/output.txt")
    writeLines(paste0("\n\nForecasting ", x, "\n\n"), file_conn)
    
    set_date_time()
    
    # cat(paste0("\n\nForecasting ", x, "\n\n"))
    
    tmp_input_frame <- input_frame %>%
      filter(date < x)
    tmp_forecast_frame <- input_frame %>%
      mutate(trend = 1:n(),
             dum_week = ifelse(format(date, "%a") == "Sat" | 
                                 format(date, "%a") == "Sun", 0, 1)) %>% 
      filter(date == x)
    
    cost <- max(abs(tmp_input_frame$price))
    
    trend_seas_fit <- seasonal_filter(tmp_input_frame)
    deseason_data_frame <- tmp_input_frame %>% 
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
                     trend_seas_fit[7] * dum_week)) %>% 
      select(-trend, -dum_week)
    
    deseason_filtered_frame <-  outlier_filt(deseason_data_frame, 3)
    
    if (forecast_type == "ar") {
      forecast <- arima(deseason_filtered_frame$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        optim.control = list(maxit = 2000)) %>%
        predict(n.ahead = h) %>%
        use_series(pred) %>%
        as.numeric
    } else if (forecast_type == "arima") {
      forecast <- arima(deseason_filtered_frame$price,
                        order = if (country == "de" & market == "intraday") c(7, 0, 7) else c(7, 0, 6),
                        method = "ML",
                        optim.control = list(maxit = 2000)) %>%
        predict(n.ahead = h) %>%
        use_series(pred) %>%
        as.numeric
    } else if (forecast_type == "arx_1") {
      forecast <- arima(deseason_filtered_frame$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = tmp_input_frame$residual_load,
                        optim.control = list(maxit = 2000)) %>%
        predict(n.ahead = h,
                newxreg = tmp_forecast_frame$residual_load) %>%
        use_series(pred) %>%
        as.numeric
    } else if (forecast_type == "arx_2") {
      forecast <- arima(deseason_filtered_frame$price,
                        order = c(7, 0, 0),
                        method = "ML",
                        xreg = tmp_input_frame$diff_residual_load,
                        optim.control = list(maxit = 2000)) %>%
        predict(n.ahead = h,
                newxreg = tmp_forecast_frame$diff_residual_load) %>%
        use_series(pred) %>%
        as.numeric
    } else if (forecast_type == "garch") {
      forecast <- 
        tryCatch({ugarchspec(variance.model = list(model = "sGARCH",
                                                   garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(7, 0)),
                             distribution = "norm") %>%
            ugarchfit(spec = .,
                      data = deseason_filtered_frame$price,
                      solver = "nloptr") %>%
            ugarchforecast(fitORspec = .,
                           n.ahead = h) %>%
            fitted %>%
            as.numeric},
            warning = function(w) {
              "warning"
            },
            error = function(e) {
              "error"
            }) %>%
        {
          if ((.) == "error") {
            NA
          } else if ((.) == "warning") {
            NA
          } else {
            (.)
          }
        }
    } else if (forecast_type == "garchx_1") {
      forecast <- 
        tryCatch({ugarchspec(variance.model = list(model = "sGARCH",
                                                   garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(7, 0),
                                               external.regressors = tmp_input_frame$residual_load %>% as.matrix),
                             distribution = "norm") %>%
            ugarchfit(spec = .,
                      data = deseason_filtered_frame$price,
                      solver = "nloptr") %>% 
            ugarchforecast(fitORspec = .,
                           n.ahead = h,
                           external.forecasts = list(tmp_forecast_frame$residual_load)) %>%
            fitted %>%
            as.numeric},
              warning = function(w) {
                "warning"
              },
              error = function(e) {
                "error"
              }) %>%
          {
            if (. == "error") {
              NA
            } else if (. == "warning") {
              NA
            } else {
              (.)
            }
          }
    } else if (forecast_type == "garchx_2") {
      forecast <- 
        tryCatch({ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(7, 0),
                                               external.regressors = tmp_input_frame$diff_residual_load %>% as.matrix),
                             distribution = "norm") %>%
            ugarchfit(spec = .,
                      data = deseason_filtered_frame$price,
                      solver = "nloptr") %>%
            ugarchforecast(fitORspec = .,
                           n.ahead = h,
                           external.forecasts = list(tmp_forecast_frame$diff_residual_load)) %>%
            fitted %>%
            as.numeric},
              warning = function(w) {
                "warning"
              },
              error = function(e) {
                "error"
              }) %>%
              {
                if (. == "error") {
                  NA
                } else if (. == "warning") {
                  NA
                } else {
                  (.)
                }
              }
    } else if (forecast_type == "svm_linear") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7))
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6)) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.5343 else epsilon = 0.9434
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.5258 else epsilon = 0.5955
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "linear",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_linearx_1") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7),
                              x8 = tmp_input_frame$residual_load)
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6),
                                  x8 = tmp_forecast_frame$residual_load) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.6701 else epsilon = 0.7321
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.5726 else epsilon = 0.4478
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "linear",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_linearx_2") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7),
                              x8 = tmp_input_frame$diff_residual_load)
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6),
                                  x8 = tmp_forecast_frame$diff_residual_load) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.6701 else epsilon = 0.8858
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.7146 else epsilon = 0.7613
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "linear",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_polynomial") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7))
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6)) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.6194 else epsilon = 0.6827
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.5055 else epsilon = 0.4300
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "polynomial",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_polynomialx_1") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7),
                              x8 = tmp_input_frame$residual_load)
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6),
                                  x8 = tmp_forecast_frame$residual_load) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.3069 else epsilon = 0.6827
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.4327 else epsilon = 0.4335
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "polynomial",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    } else if (forecast_type == "svm_polynomialx_2") {
      svm_input <- data.frame(y = deseason_filtered_frame$price,
                              x1 = lag(deseason_filtered_frame$price, 1),
                              x2 = lag(deseason_filtered_frame$price, 2),
                              x3 = lag(deseason_filtered_frame$price, 3),
                              x4 = lag(deseason_filtered_frame$price, 4),
                              x5 = lag(deseason_filtered_frame$price, 5),
                              x6 = lag(deseason_filtered_frame$price, 6),
                              x7 = lag(deseason_filtered_frame$price, 7),
                              x8 = tmp_input_frame$diff_residual_load)
      
      svm_for_input <- data.frame(x1 = deseason_filtered_frame$price,
                                  x2 = lag(deseason_filtered_frame$price, 1),
                                  x3 = lag(deseason_filtered_frame$price, 2),
                                  x4 = lag(deseason_filtered_frame$price, 3),
                                  x5 = lag(deseason_filtered_frame$price, 4),
                                  x6 = lag(deseason_filtered_frame$price, 5),
                                  x7 = lag(deseason_filtered_frame$price, 6),
                                  x8 = tmp_forecast_frame$diff_residual_load) %>%
        tail(1)
      
      if (country == "de") {
        if (market == "spot") epsilon = 0.6199 else epsilon = 0.6827
      } else if (country == "fr") {
        if (market == "spot") epsilon = 0.3635 else epsilon = 0.5948
      }
      
      forecast <- svm(y ~ .,
                      data = svm_input,
                      kernel = "polynomial",
                      scale = TRUE,
                      type = "eps-regression",
                      cost = cost,
                      epsilon = epsilon,
                      cachesize = 4000) %>%
        predict(newdata = svm_for_input) %>%
        as.numeric
    }
    
    forecast_out <- data.frame(
      date = x,
      type = forecast_type,
      market = market,
      country = country,
      forecast = (forecast +
                    (trend_seas_fit[1] + trend_seas_fit[2] * tmp_forecast_frame$trend +
                       trend_seas_fit[3]  * sin((tmp_forecast_frame$trend + trend_seas_fit[4])
                                                * 2 * pi / 365) +
                       trend_seas_fit[5] * sin((tmp_forecast_frame$trend + trend_seas_fit[6])
                                               * 4 * pi / 365) +
                       trend_seas_fit[7] * tmp_forecast_frame$dum_week)))
    
    writeLines(paste0("\n\nForecast: ", forecast_out, "\n\n"), file_conn)
    close(file_conn)
    # cat("\n\nForecast: \n\n")
    # print(forecast_out)
    
    return(forecast_out)
  }) %>% 
    rbind_all
  
  stopCluster(cl)
  
  write.csv(fore_out, paste0("C:/git/r/powerfor/inst/forecasts/", country, "_", 
                             market, "_forecast_", forecast_type, ".csv"))
}