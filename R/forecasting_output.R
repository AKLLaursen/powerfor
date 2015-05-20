#' Read and munge forecasting data
#' 
#' @export
#' 
read_and_munge_forecast <- function() {
  files <- list.files("/inst/forecasts/base_load")
  
  de_spot_true <- readRDS("/inst/rds/data_de_spot.rds") %>%
    group_by(date) %>%
    summarise(price = mean(price)) %>%
    ungroup
  de_intraday_true <- readRDS("/inst/rds/data_de_intraday.rds") %>%
    group_by(date) %>%
    summarise(price = mean(price)) %>%
    ungroup
  fr_spot_true <- readRDS("/inst/rds/data_fr_spot.rds") %>%
    group_by(date) %>%
    summarise(price = mean(price)) %>%
    ungroup
  fr_intraday_true <- readRDS("/inst/rds/data_fr_intraday.rds") %>%
    group_by(date) %>%
    summarise(price = mean(price)) %>%
    ungroup
  
  de_spot <- files[grepl("de_spot", files)]
  de_intraday <- files[grepl("de_intraday", files)]
  fr_spot <- files[grepl("fr_spot", files)]
  fr_intraday <- files[grepl("fr_intraday", files)]
  
  forecast <- list(
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/base_load", de_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
      }) %>%
      rbind_all %>%
      left_join(de_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/base_load", de_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(de_intraday_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/base_load", fr_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
      }) %>%
      rbind_all %>%
      left_join(fr_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/base_load", fr_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
      }) %>%
      rbind_all %>%
      left_join(fr_intraday_true, by = c("date"))) %>%
    rbind_all
  
#   forecast %<>%
#     mutate(forecast = ifelse(forecast > 100 | forecast < -50, NA, forecast))
  
  return(forecast)
}

#' @export
calc_stat_loss <- function(path = NULL) {
  
  data_frame <- read_and_munge_forecast()
  
  data_frame %<>%
    group_by(country, market, type) %>%
    summarise(MSE = mean((forecast - price) ^ 2),
              MAE = mean(abs(forecast - price)),
              MAPE = mean(abs((forecast - price) / price)) * 100,
              sMAPE = mean(abs((forecast - price) /
                                 (price + forecast))) * 100) %>%
    ungroup %>%
    mutate(type = ifelse(type == "ar", "AR",
                         ifelse(type == "arima", "ARMA",
                                ifelse(type == "arx_1", "ARX - level",
                                       ifelse(type == "arx_2", "ARX - diff",
                                              ifelse(type == "esc1", "Escribano a",
                                                     ifelse(type == "esc2", "Escribano b",
                                                            ifelse(type == "esc3", "Escribano c",
                                                                   ifelse(type == "esc4", "Escribano - AKLL",
                                                                          ifelse(type == "garch", "GARCH",
                                                                                 ifelse(type == "garchx_1", "GARCHX - level",
                                                                                        ifelse(type == "garchx_2", "GARCHX - diff",
                                                                                               ifelse(type == "svm_linear", "SVM - linear",
                                                                                                      ifelse(type == "svm_linearx_1", "SVM - linear - level",
                                                                                                             ifelse(type == "svm_linearx_2", "SVM - linear - diff",
                                                                                                                    ifelse(type == "svm_polynomial", "SVM - polynomial",
                                                                                                                           ifelse(type == "svm_polynomialx_1", "SVM - polynomial - level",
                                                                                                                                  ifelse(type == "svm_polynomialx_2", "SVM - polynomial - diff", type))))))))))))))))))
  de_out <- data_frame %>% 
    filter(country == "de") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  fr_out <- data_frame %>%
    filter(country == "fr") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  
  if (!is.null(path)) {
    xtable(de_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/de_stat_loss.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
    xtable(fr_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/fr_stat_loss.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
calc_profit <- function(data_frame) {
  data_frame %<>%
    unite_("market_comb", c("country", "market"), remove = FALSE) %>% 
    select(-market, -country) %>%
    arrange(date, type, market_comb) %>%
    group_by(type, date) %>%
    summarise(profit = price[which(forecast == max(forecast))] -
                price[head(which(forecast == min(forecast)), 1)]) %>%
    ungroup
}

calc_eco_loss <- function(path = NULL) {
  data_frame <- read_and_munge_forecast_hour_10()
  
  data_frame %<>% calc_profit
  
  profit <- data_frame %>% 
    group_by(type) %>%
    summarise(`Total profit` = sum(profit),
              `Average profit` = mean(profit),
              `Std.dev.` = sd(profit)) %>%
    ungroup %>% 
    mutate(type = ifelse(type == "ar", "AR",
                         ifelse(type == "arima", "ARMA",
                                ifelse(type == "arx_1", "ARX - level",
                                       ifelse(type == "arx_2", "ARX - diff",
                                              ifelse(type == "esc1", "Escribano a",
                                                     ifelse(type == "esc2", "Escribano b",
                                                            ifelse(type == "esc3", "Escribano c",
                                                                   ifelse(type == "esc4", "Escribano - AKLL",
                                                                          ifelse(type == "garch", "GARCH",
                                                                                 ifelse(type == "garchx_1", "GARCHX - level",
                                                                                        ifelse(type == "garchx_2", "GARCHX - diff",
                                                                                               ifelse(type == "svm_linear", "SVM - linear",
                                                                                                      ifelse(type == "svm_linearx_1", "SVM - linear - level",
                                                                                                             ifelse(type == "svm_linearx_2", "SVM - linear - diff",
                                                                                                                    ifelse(type == "svm_polynomial", "SVM - polynomial",
                                                                                                                           ifelse(type == "svm_polynomialx_1", "SVM - polynomial - level",
                                                                                                                                  ifelse(type == "svm_polynomialx_2", "SVM - polynomial - diff", type))))))))))))))))))
  
  
  data_frame %<>% 
    mutate(type = ifelse(type == "ar", "AR",
                         ifelse(type == "arima", "ARMA",
                                ifelse(type == "arx_1", "ARX - level",
                                       ifelse(type == "arx_2", "ARX - diff",
                                              ifelse(type == "esc1", "Escribano a",
                                                     ifelse(type == "esc2", "Escribano b",
                                                            ifelse(type == "esc3", "Escribano c",
                                                                   ifelse(type == "esc4", "Escribano - AKLL",
                                                                          ifelse(type == "garch", "GARCH",
                                                                                 ifelse(type == "garchx_1", "GARCHX - level",
                                                                                        ifelse(type == "garchx_2", "GARCHX - diff",
                                                                                               ifelse(type == "svm_linear", "SVM - linear",
                                                                                                      ifelse(type == "svm_linearx_1", "SVM - linear - level",
                                                                                                             ifelse(type == "svm_linearx_2", "SVM - linear - diff",
                                                                                                                    ifelse(type == "svm_polynomial", "SVM - polynomial",
                                                                                                                           ifelse(type == "svm_polynomialx_1", "SVM - polynomial - level",
                                                                                                                                  ifelse(type == "svm_polynomialx_2", "SVM - polynomial - diff", type))))))))))))))))))
  
  
  profit_plot <- data_frame %>%
    group_by(type) %>% 
    mutate(agg_prof = cumsum(profit)) %>%
    ungroup %>%
    draw_profit()
  
  if (!is.null(path)) {
    xtable(profit,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/simple_profit_hour_10.tex"),
            floating = FALSE,
            include.rownames = FALSE)
  }
}

#' Read and munge forecasting data
#' 
#' @export
#' 
read_and_munge_forecast_hour_4 <- function() {
  files <- list.files("/inst/forecasts/hour_4")
  
  de_spot_true <- readRDS("/inst/rds/data_de_spot.rds") %>%
    filter(hour == 4)
  de_intraday_true <- readRDS("/inst/rds/data_de_intraday.rds") %>%
    filter(hour == 4)
  fr_spot_true <- readRDS("/inst/rds/data_fr_spot.rds") %>%
    filter(hour == 4)
  fr_intraday_true <- readRDS("/inst/rds/data_fr_intraday.rds") %>%
    filter(hour == 4)
  
  de_spot <- files[grepl("de_spot", files)]
  de_intraday <- files[grepl("de_intraday", files)]
  fr_spot <- files[grepl("fr_spot", files)]
  fr_intraday <- files[grepl("fr_intraday", files)]
  
  forecast <- list(
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_4", de_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(de_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_4", de_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(de_intraday_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_4", fr_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(fr_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_4", fr_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(fr_intraday_true, by = c("date"))) %>%
    rbind_all
  
  #   forecast %<>%
  #     mutate(forecast = ifelse(forecast > 100 | forecast < -50, NA, forecast))
  
  return(forecast)
}

calc_stat_loss_4 <- function(path = NULL) {
  
  data_frame <- read_and_munge_forecast_hour_4()
  
  data_frame %<>%
    group_by(country, market, type) %>%
    mutate(MSE = mean((forecast - price) ^ 2),
              MAE = mean(abs(forecast - price)),
              MAPE = mean(abs((forecast - price) / price)) * 100,
              sMAPE = mean(abs((forecast - price) /
                                 (price + forecast))) * 100) %>%
    ungroup %>%
    mutate(type = ifelse(type == "ar", "AR",
                         ifelse(type == "arima", "ARMA",
                                ifelse(type == "arx_1", "ARX - level",
                                       ifelse(type == "arx_2", "ARX - diff",
                                              ifelse(type == "esc1", "Escribano a",
                                                     ifelse(type == "esc2", "Escribano b",
                                                            ifelse(type == "esc3", "Escribano c",
                                                                   ifelse(type == "esc4", "Escribano - AKLL",
                                                                          ifelse(type == "garch", "GARCH",
                                                                                 ifelse(type == "garchx_1", "GARCHX - level",
                                                                                        ifelse(type == "garchx_2", "GARCHX - diff",
                                                                                               ifelse(type == "svm_linear", "SVM - linear",
                                                                                                      ifelse(type == "svm_linearx_1", "SVM - linear - level",
                                                                                                             ifelse(type == "svm_linearx_2", "SVM - linear - diff",
                                                                                                                    ifelse(type == "svm_polynomial", "SVM - polynomial",
                                                                                                                           ifelse(type == "svm_polynomialx_1", "SVM - polynomial - level",
                                                                                                                                  ifelse(type == "svm_polynomialx_2", "SVM - polynomial - diff", type))))))))))))))))))
  
  
  de_out <- data_frame %>% 
    filter(country == "de") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  fr_out <- data_frame %>%
    filter(country == "fr") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  
  if (!is.null(path)) {
    xtable(de_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/de_stat_loss_4.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
    xtable(fr_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/fr_stat_loss_4.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' Read and munge forecasting data
#' 
#' @export
#' 
read_and_munge_forecast_hour_10 <- function() {
  files <- list.files("/inst/forecasts/hour_10")
  
  de_spot_true <- readRDS("/inst/rds/data_de_spot.rds") %>%
    filter(hour == 10)
  de_intraday_true <- readRDS("/inst/rds/data_de_intraday.rds") %>%
    filter(hour == 10)
  fr_spot_true <- readRDS("/inst/rds/data_fr_spot.rds") %>%
    filter(hour == 10)
  fr_intraday_true <- readRDS("/inst/rds/data_fr_intraday.rds") %>%
    filter(hour == 10)
  
  de_spot <- files[grepl("de_spot", files)]
  de_intraday <- files[grepl("de_intraday", files)]
  fr_spot <- files[grepl("fr_spot", files)]
  fr_intraday <- files[grepl("fr_intraday", files)]
  
  forecast <- list(
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_10", de_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(de_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_10", de_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(de_intraday_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_10", fr_spot[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(fr_spot_true, by = c("date")),
    lapply(1:length(de_spot), function(x) {
      read.csv(paste0("/inst/forecasts/hour_10", fr_intraday[x]),
               stringsAsFactors = FALSE,
               row.names = 1) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d"))
    }) %>%
      rbind_all %>%
      left_join(fr_intraday_true, by = c("date"))) %>%
    rbind_all
  
  #   forecast %<>%
  #     mutate(forecast = ifelse(forecast > 100 | forecast < -50, NA, forecast))
  
  return(forecast)
}

calc_stat_loss_10 <- function(path = NULL) {
  
  data_frame <- read_and_munge_forecast_hour_10()
  
  data_frame %<>%
    group_by(country, market, type) %>%
    summarise(MSE = mean((forecast - price) ^ 2),
              MAE = mean(abs(forecast - price)),
              MAPE = mean(abs((forecast - price) / price)) * 100,
              sMAPE = mean(abs((forecast - price) /
                                 (price + forecast))) * 100) %>%
    ungroup %>%
    mutate(type = ifelse(type == "ar", "AR",
                         ifelse(type == "arima", "ARMA",
                                ifelse(type == "arx_1", "ARX - level",
                                       ifelse(type == "arx_2", "ARX - diff",
                                              ifelse(type == "esc1", "Escribano a",
                                                     ifelse(type == "esc2", "Escribano b",
                                                            ifelse(type == "esc3", "Escribano c",
                                                                   ifelse(type == "esc4", "Escribano - AKLL",
                                                                          ifelse(type == "garch", "GARCH",
                                                                                 ifelse(type == "garchx_1", "GARCHX - level",
                                                                                        ifelse(type == "garchx_2", "GARCHX - diff",
                                                                                               ifelse(type == "svm_linear", "SVM - linear",
                                                                                                      ifelse(type == "svm_linearx_1", "SVM - linear - level",
                                                                                                             ifelse(type == "svm_linearx_2", "SVM - linear - diff",
                                                                                                                    ifelse(type == "svm_polynomial", "SVM - polynomial",
                                                                                                                           ifelse(type == "svm_polynomialx_1", "SVM - polynomial - level",
                                                                                                                                  ifelse(type == "svm_polynomialx_2", "SVM - polynomial - diff", type))))))))))))))))))
  
  
  de_out <- data_frame %>% 
    filter(country == "de") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  fr_out <- data_frame %>%
    filter(country == "fr") %>% 
    arrange(market %>% desc) %>%
    select(-country, -market)
  
  if (!is.null(path)) {
    xtable(de_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/de_stat_loss_10.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
    xtable(fr_out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path, "/fr_stat_loss_10.tex"),
            floating = FALSE,
            sanitize.text.function = function(x){x},
            include.rownames = FALSE)
  }
}

#' @export
plot_forecast <- function(save_path = NULL,
                          do_print = TRUE) {
  
  data_base_load <- read_and_munge_forecast() %>%
    mutate(`Forecast type` = "Base load")
  data_hour_4 <- read_and_munge_forecast_hour_4() %>%
    mutate(`Forecast type` = "Hour 4") %>%
    select(-hour, -volume)
  data_base_10 <- read_and_munge_forecast_hour_10() %>%
    mutate(`Forecast type` = "Hour 10") %>%
    select(-hour, -volume)
  
  plot_data <- rbind(data_base_load, data_hour_4, data_base_10) %>%
    mutate(country = ifelse(country == "de", "Germany",
                            ifelse(country == "fr", "France", NA)),
           market = ifelse(market == "spot", "Day-ahead",
                           ifelse(market == "intraday", "Intraday", NA))) %>%
    unite(market_con, market, country, sep = " - ") %>%
    mutate(price = ifelse(`Forecast type` == "Base load", price, NA))
  
  idx <- unique(plot_data$type)
  
  lapply(idx, function(x) {
    plot_forecast(plot_data %>% filter(type == x),
                  x,
                  save_path = save_path)
  })
}