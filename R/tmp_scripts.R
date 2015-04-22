out_data <- function(test_start = "2013-01-01") {
  input_frame_exp <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_exogen.rds")
  input_frame <- readRDS("C:/git/r/powerfor/inst/rds/data_fr_spot.rds")
  
  test_start %<>% as.Date
  
  input_frame_exp %<>%
    group_by(date) %>%
    summarise(residual_load = mean(residual_load)) %>%
    ungroup %>%
    mutate(residual_load = (residual_load - mean(residual_load)) / sd(residual_load),
           diff_residual_load = residual_load - lag(residual_load, 1))
  
  input_frame %<>% 
    filter_only_outliers() %>% 
    mutate(week_dum = ifelse(format(date, "%a") == "Sat" |
                               format(date, "%a") == "Sun", 0, 1),
           seas_1 = ifelse(as.numeric(format(date, "%m")) %in% c(9, 10, 11), 1, 0),
           seas_2 = ifelse(as.numeric(format(date, "%m")) %in% c(3, 4, 5), 1, 0),
           seas_3 = ifelse(as.numeric(format(date, "%m")) %in% c(6, 7, 8), 1, 0)) %>%
    left_join(input_frame_exp, by = c("date"))
  
  date_vec <- seq(test_start, tail(input_frame$date, 1), by = "day")
  
  lapply(1:length(date_vec), function(x) {
    input_frame_tmp <- input_frame %>% 
      filter(date <= date_vec[x])
    
    write.table(input_frame_tmp[, 2:8],
                paste0("C:/git/r/powerfor/inst/csv/forecast_data_fr_spot/data_",
                       x,
                       ".csv"),
                sep = ",",
                row.names = FALSE,
                col.names = FALSE,
                na = "NaN")
  })
}

calc_stat_loss <- function() {
  
  files <- list.files("C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Forecasts")
  
  data_frame <- lapply(1:length(files), function(x) {
    read.csv(paste0("C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Forecasts/",
             files[x]),
             stringsAsFactors = FALSE)
  }) %>% rbind_all
  
  data_frame %>%
    group_by(type, market, country) %>%
    summarise(MSE = mean((forecast - price) ^ 2),
              MAE = mean(abs(forecast - price)),
              MPE = mean((forecast - price) / price) ,
              MAPE = mean(abs((forecast - price) / price)) * 100,
              sMAPE = mean(abs((forecast - price) /
                                 (price + forecast))) * 100) %>%
    ungroup
}