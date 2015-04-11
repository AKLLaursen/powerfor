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
                                  save_path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Child Documents/A_Brief_Overview_of_Electricity_Markets/Figures",
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