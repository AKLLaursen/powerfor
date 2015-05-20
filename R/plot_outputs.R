#' Function generating the plots used in the seasonality subsection of thesis
#' 
#' @param input_frame_spot A data frame with spot price data.
#' @param input_frame_intraday A data frame with spot price data.
#' @param save_path A string with the path to save plot.
#' @param do_print A boolean of whether or not to print the output.
#' 
#' @export
#' 
power_characteristics <- function(input_frame_spot,
                                  input_frame_intraday,
                                  save_path = NULL,
                                  do_print = FALSE) {
  
  input_frame_spot_long <- input_frame_spot
  input_frame_spot %<>% filter(date >= "2010-12-01")
  
  trend_seas_fit_spot <- long_run_trend_season(input_frame_spot)
  trend_seas_est_spot <- input_frame_spot %>% 
    transmute(hour = rep(1:24, n() / 24),
              date_time = date %>% as.POSIXct(tz = "Copenhagen") %>% `+`((hour - 1) * 3600),
              trend = 1:n(),
              ewma = ewma(price),
              `Day ahead price` = price,
              LRTS = (trend_seas_fit_spot[1] * 
                        sin(2 * pi * (trend / 365 + trend_seas_fit_spot[2])) -
                        trend_seas_fit_spot[3] + trend_seas_fit_spot[4] * ewma)) %>%
    select(-trend, -ewma, -hour) %>%
    gather(Series, value, `Day ahead price`, LRTS)
  
  trend_seas_fit_intraday <- long_run_trend_season(input_frame_intraday)
  trend_seas_est_intraday <- input_frame_intraday %>% 
    transmute(hour = rep(1:24, n() / 24),
              date_time = date %>% as.POSIXct() %>% `+`((hour - 1) * 3600),
              trend = 1:n(),
              ewma = ewma(price),
              `Intraday price` = price,
              LRTS = (trend_seas_fit_intraday[1] *
                        sin(2 * pi * (trend / 365 + trend_seas_fit_intraday[2])) -
                        trend_seas_fit_intraday[3] + trend_seas_fit_intraday[4] * ewma)) %>%
    select(-trend, -ewma, -hour) %>%
    gather(Series, value, `Intraday price`, LRTS)
  
  plot_lrts(trend_seas_est_spot, trend_seas_est_intraday,
            file_name = "2_3_seasonalities.eps",
            save_path,
            do_print = do_print)
  
  date_vec <- seq("2014-08-04" %>% as.POSIXct,
                  "2014-08-11" %>% as.POSIXct,
                  by = "hour") %>%
    head(-1)
  
  plot_srs(trend_seas_est_spot %>%
             filter(date_time %in% date_vec & Series == "Day ahead price"),
           trend_seas_est_intraday %>%
             filter(date_time %in% date_vec & Series == "Intraday price"),
           file_name = "2_3_seasonalities_short.eps",
           save_path,
           do_print = do_print)
  
  plot_spike(trend_seas_est_spot %>% filter(Series == "Day ahead price"),
             trend_seas_est_intraday %>% filter(Series == "Intraday price"),
             file_name = "2_3_spikes.eps",
             save_path,
             do_print = do_print)
  
  input_frame_spot_long %<>% transmute(hour = rep(1:24, n() / 24),
                                       date_time = date %>% as.POSIXct(tz = "Copenhagen") %>% `+`((hour - 1) * 3600),
                                       value = price)
  
  plot_neg(input_frame_spot_long,
           file_name = "2_3_negatives.eps",
           save_path,
           do_print = do_print)
  
}

#' @export
desc_stat_and_plots_spot <- function(path_tables = NULL, path_figures = NULL) {
  input_frame_de <- readRDS("/inst/rds/data_de_spot.rds")
  input_frame_fr <- readRDS("/inst/rds/data_fr_spot.rds")
  
  input_frame_de %<>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  input_frame_fr %<>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  
  de_desc <- input_frame_de %>%
    summarise(n = n(),
              Mean = mean(price),
              Min = min(price),
              Max = max(price),
              `Std.dev.` = sd(price),
              Skew = skewness(price),
              Kurt = kurtosis(price))
  
  fr_desc <- input_frame_fr %>%
    summarise(n = n(),
              Mean = mean(price),
              Min = min(price),
              Max = max(price),
              `Std.dev.` = sd(price),
              Skew = skewness(price),
              Kurt = kurtosis(price))
  
  out <- rbind(de_desc, fr_desc)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path_tables, "/spot_desc.tex"),
            floating = FALSE,
            include.rownames = FALSE)
  }
  
  draw_multi_line_plot(input_frame_de, input_frame_fr, "Year", "Eur/MWh", "price", file_name = "spot_series.eps",
                  save_path = path_figures, do_print = TRUE)
  draw_density(input_frame_de, input_frame_fr, "Eur/MWh", "Density", "price", file_name = "spot_density.eps",
               save_path = path_figures, do_print = TRUE)
  draw_acf_and_per(input_frame_de, lags = 28, input = "price", file_name = "de_spot_afc.eps",
                   save_path = path_figures,
                   do_print = TRUE)
  draw_acf_and_per(input_frame_fr, lags = 28, input = "price", file_name = "fr_spot_afc.eps",
                   save_path = path_figures,
                   do_print = TRUE)
  
  input_frame_de_filtered <- input_frame_de %>%
    pre_model_processing()
  input_frame_fr_filtered <- input_frame_fr %>% 
    pre_model_processing()
  
  draw_multi_line_plot(input_frame_de_filtered, input_frame_fr_filtered, "Year", "Eur/MWh", "price", file_name = "filtered_spot_series.eps",
                       save_path = path_figures, do_print = TRUE)
  draw_density(input_frame_de_filtered, input_frame_fr_filtered, "Eur/MWh", "Density", "price", file_name = "filtered_spot_density.eps",
               save_path = path_figures, do_print = TRUE)
  draw_acf_and_per(input_frame_de_filtered, lags = 28, input = "price", file_name = "de_filtered_spot_afc.eps",
                   save_path = path_figures,
                   do_print = TRUE)
  draw_acf_and_per(input_frame_fr_filtered, lags = 28, input = "price", file_name = "fr_filtered_spot_afc.eps",
                   save_path = path_figures,
                   do_print = TRUE)
}

#' @export
desc_stat_and_plots_intraday <- function(path_table = NULL,
                                         path_figure = NULL) {
  input_frame_de <- readRDS("/inst/rds/data_de_intraday.rds")
  input_frame_fr <- readRDS("/inst/rds/data_fr_intraday.rds")
  
  input_frame_de %<>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  input_frame_fr %<>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  
  de_desc <- input_frame_de %>%
    summarise(n = n(),
              Mean = mean(price),
              Min = min(price),
              Max = max(price),
              `Std.dev.` = sd(price),
              Skew = skewness(price),
              Kurt = kurtosis(price))
  
  fr_desc <- input_frame_fr %>%
    summarise(n = n(),
              Mean = mean(price),
              Min = min(price),
              Max = max(price),
              `Std.dev.` = sd(price),
              Skew = skewness(price),
              Kurt = kurtosis(price))
  
  out <- rbind(de_desc, fr_desc)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path_table, "/intraday_desc.tex"),
            floating = FALSE,
            include.rownames = FALSE)
  }
  
  draw_multi_line_plot(input_frame_de, input_frame_fr, "Year", "Eur/MWh", "price", file_name = "intraday_series.eps",
                       save_path = path_figure, do_print = TRUE)
  draw_density(input_frame_de, input_frame_fr, "Eur/MWh", "Density", "price", file_name = "intraday_density.eps",
               save_path = path_figure, do_print = TRUE)
  draw_acf_and_per(input_frame_de, lags = 28, input = "price", file_name = "de_intraday_afc.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  draw_acf_and_per(input_frame_fr, lags = 28, input = "price", file_name = "fr_intraday_afc.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  
  input_frame_de_filtered <- input_frame_de %>%
    pre_model_processing()
  input_frame_fr_filtered <- input_frame_fr %>% 
    pre_model_processing()
  
  draw_multi_line_plot(input_frame_de_filtered, input_frame_fr_filtered, "Year", "Eur/MWh", "price", file_name = "filtered_intraday_series.eps",
                       save_path = path_figure, do_print = TRUE)
  draw_density(input_frame_de_filtered, input_frame_fr_filtered, "Eur/MWh", "Density", "price", file_name = "filtered_intraday_density.eps",
               save_path = path_figure, do_print = TRUE)
  draw_acf_and_per(input_frame_de_filtered, lags = 28, input = "price", file_name = "de_filtered_intraday_afc.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  draw_acf_and_per(input_frame_fr_filtered, lags = 28, input = "price", file_name = "fr_filtered_intraday_afc.eps",
                   save_path = path_figure,
                   do_print = TRUE)
  
  input_frame_exo_de <- readRDS("/inst/rds/data_de_exogen.rds") %>% 
    group_by(date) %>%
    summarise(cons_forecast = mean(cons_forecast),
              wind_forecast = mean(wind_forecast),
              solar_forecast = mean(solar_forecast),
              residual_load = mean(residual_load)) %>%
    ungroup
  input_frame_exo_fr <- readRDS("/inst/rds/data_fr_exogen.rds") %>% 
    group_by(date) %>%
    summarise(cons_forecast = mean(cons_forecast),
              wind_forecast = mean(wind_forecast),
              residual_load = mean(residual_load)) %>%
    ungroup
  
  input_frame_de_spot <- readRDS("/inst/rds/data_de_intraday.rds") %>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  input_frame_fr_spot <- readRDS("/inst/rds/data_fr_intraday.rds") %>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  
  input_frame_de_intraday <- readRDS("/inst/rds/data_de_intraday.rds") %>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  input_frame_fr_intraday <- readRDS("/inst/rds/data_fr_intraday.rds") %>%
    group_by(date) %>% 
    summarise(price = mean(price)) %>% 
    ungroup()
  
  draw_multi_line_plot_de(input_frame_exo_de, file_name = "de_exogen.eps",
                          save_path = path_figure,
                          do_print = TRUE)
  draw_multi_line_plot_fr(input_frame_exo_fr, file_name = "fr_exogen.eps",
                          save_path = path_figure,
                          do_print = TRUE)
  
  draw_scatter(input_frame_de_spot, input_frame_de_intraday, input_frame_exo_de, country = "de",
               save_path = path_figure,
               do_print = TRUE)
  
  draw_scatter(input_frame_fr_spot, input_frame_fr_intraday, input_frame_exo_fr, country = "fr",
               save_path = path_figure,
               do_print = TRUE)
  
  de_exo_desc <- input_frame_exo_de %>%
    summarise(n = n(),
              Mean = mean(residual_load),
              Min = min(residual_load),
              Max = max(residual_load),
              `Std.dev.` = sd(residual_load),
              Skew = skewness(residual_load),
              Kurt = kurtosis(residual_load))
  
  fr_exo_desc <- input_frame_exo_fr %>%
    summarise(n = n(),
              Mean = mean(residual_load),
              Min = min(residual_load),
              Max = max(residual_load),
              `Std.dev.` = sd(residual_load),
              Skew = skewness(residual_load),
              Kurt = kurtosis(residual_load))
  
  out <- rbind(de_exo_desc, fr_exo_desc)
  
  if (!is.null(path)) {
    xtable(out,
           digits = 2) %>%
      print(type = "latex",
            file = paste0(path_table, "/residual_load_desc.tex"),
            floating = FALSE,
            include.rownames = FALSE)
  }
  
  draw_density(input_frame_exo_de, input_frame_exo_fr, "Residual load, MWh", "Density", "residual_load", file_name = "residual_load_density.eps",
               save_path = path_figure, do_print = TRUE)
}