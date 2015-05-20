#' Flexible function for plotting line chart of data
#' 
#' @param input_frame A dataframe with a date object and a series to be plotted
#' @param xlabel A string containing x-label name
#' @param ylabel A string containing y-label name
#' @param input A string with input variable name
#' @param file_name A string with filename wanted for output plot
#' @param save_path A string with path to save output
#' @param do_print A boloean of wether or not to print the output
#' 
#' @export
#' 
draw_line_plot <- function(input_frame, xlabel, ylabel, input, file_name = NULL,
                           save_path = NULL, do_print = TRUE) {
  p <- ggplot(input_frame, aes_string(x = "date", y = input)) +
    geom_line(color = "#003366", size = 0.2) +
    xlab(xlabel) +
    ylab(ylabel) +
    theme(axis.line = element_line(colour = "#E0E0DF"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(colour = "#656560"),
          axis.title.y = element_text(colour = "#656560"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E0E0DF"),
          panel.grid.minor.y = element_line(colour = "#E0E0DF"),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank())
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Flexible function for plotting autocorrelation functions in ggplot
#' 
#' @param input_frame A dataframe with a date object and a series to be plotted
#' @param lags An integer giving the number of lags
#' @param input A string with input variable name
#' @param file_name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
draw_acf <- function(input_frame, lags, input, file_name = NULL,
                     save_path = NULL, do_print = TRUE) {
  stats_acf <- input_frame %>% `[`(, input) %>%
    acf(lag.max = lags, plot = FALSE) %>%
    with(data.frame(lag, acf))
  
  stats_pacf <- input_frame %>% `[`(, input) %>%
    pacf(lag.max = lags, plot = FALSE) %>%
    with(data.frame(lag, acf))
  
  acf_sig_level <- qnorm((1 + 0.95) / 2) / 
    sqrt(sum(!is.na(input_frame$price)))
  
  plots <- list(
    p1 = ggplot(stats_acf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_hline(yintercept = -acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_segment(aes(xend = lag, yend = 0)) +
      ylim(0, 1) +
      xlab("Lag number") +
      ylab("Autocorrelation"),
    p2 = ggplot(stats_pacf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_hline(yintercept = -acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_segment(aes(xend = lag, yend = 0)) +
      ylim(0, 1) +
      xlab("Lag number") +
      ylab("Autocorrelation"))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Function plotting the periodogram for a univariate time series
#' 
#' @param input_frame A dataframe with a series named price
#' @param log A bolean indicating if y-axis shcould be logarithmic
#' @param input A string with input variable name
#' @param file name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
draw_periodogram <- function(input_frame, input, log = TRUE, file_name = NULL,
                             save_path = NULL, do_print = TRUE) {
  period <- spec.pgram(input_frame %>% `[`(, input),
                       taper = 0,
                       detrend = FALSE,
                       demean = FALSE,
                       plot = TRUE) %>%
    with(data.frame(spec = spec, freq = freq))
  
  if (log) {
    p <- ggplot(period, aes(x = freq, y = spec)) +
      geom_line(color = "#003366") +
      scale_y_log10() +
      xlab("Frequency") +
      ylab("Spectrum")
  } else {
    p <- ggplot(period, aes(x = freq, y = spec)) +
      geom_line(color = "#003366") +
      xlab("Frequency") +
      ylab("Spectrum")
  }
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Function plotting seasonalities of intraday and dayahed data for a given
#' interval, long run
#' 
#' @param input_frame A dataframe with a series named price
#' @param file_name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
plot_lrts <- function(input_frame_spot, input_frame_intraday, file_name,
                      save_path, do_print = FALSE) {
  plots <- list(
    p1 = ggplot(input_frame_spot,
                aes(x = date_time, y = value, color = Series)) +
      geom_line() +
      scale_colour_manual(values = c("#003366", "#E4001B")) +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Day-ahead price") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_intraday,
                aes(x = date_time, y = value, color = Series)) +
      geom_line() +
      scale_colour_manual(values = c("#003366", "#E4001B")) +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Intraday VWAP") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Function plotting seasonalities of intraday and dayahed data for a given
#' interval, short run
#' 
#' @param input_frame A dataframe with a series named price
#' @param file_name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
plot_srs <- function(input_frame_spot, input_frame_intraday, file_name,
                      save_path, do_print = FALSE) {
  plots <- list(
    p1 = ggplot(input_frame_spot,
                aes(x = date_time, y = value)) +
      geom_line(color = "#003366") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Day-ahead price") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_intraday,
                aes(x = date_time, y = value)) +
      geom_line(color = "#003366") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Intraday VWAP") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Function plotting spikes of intraday and dayahed data for a given
#' interval.
#' 
#' @param input_frame A dataframe with a series named price
#' @param file_name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
plot_spike <- function(input_frame_spot, input_frame_intraday, file_name,
                     save_path, do_print = FALSE) {
  
  input_frame_spot_tmp <- input_frame_spot %>% 
    filter(value > mean(value) + 3 * sd(value) |
             value < mean(value) - 3 * sd(value))
  input_frame_intraday_tmp <- input_frame_intraday %>%
    filter(value > mean(value) + 3 * sd(value) |
             value < mean(value) - 3 * sd(value))
  
  plots <- list(
    p1 = ggplot(input_frame_spot,
                aes(x = date_time, y = value)) +
      geom_line(color = "#003366") +
      geom_point(data = input_frame_spot_tmp, aes(x = date_time, y = value),
                 colour = "#E4001B", shape = 1, size = 3) +
      geom_hline(aes(yintercept = mean(value)),
                 colour = "#E4001B") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Day-ahead price") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_spot,
                aes(x = date_time, y = value)) +
      geom_line(color = "#003366") +
      geom_point(data = input_frame_spot_tmp, aes(x = date_time, y = value),
                 colour = "#E4001B", shape = 1, size = 3) +
      geom_hline(aes(yintercept = mean(value)),
                 colour = "#E4001B") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Intraday VWAP") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' Function plotting negative prices for dayahed data.
#' 
#' @param input_frame A dataframe with a series named price
#' @param file_name A string indicating name of output
#' @param save_path A string with path to save file
#' @param do_print A Bolean indicating if plot should be printed
#' 
#' @export
#' 
plot_neg <- function(input_frame_spot, file_name, save_path, do_print = FALSE) {
  
  input_frame_spot_tmp <- input_frame_spot %>% 
    filter(value < 0)
  
  p <- ggplot(input_frame_spot, aes(x = date_time, y = value)) +
    geom_line(color = "#003366") +
    geom_point(data = input_frame_spot_tmp, aes(x = date_time, y = value),
               colour = "#E4001B", shape = 1, size = 3) +
    geom_hline(aes(yintercept = 0),
               colour = "#E4001B") +
    xlab("Time") +
    ylab("Price, EUR/MWh") +
    ggtitle("Day-ahead price") +
    theme(axis.line = element_line(colour = "#E0E0DF"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(colour = "#656560"),
          axis.title.y = element_text(colour = "#656560"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E0E0DF"),
          panel.grid.minor.y = element_line(colour = "#E0E0DF"),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank())
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
plot_escribiano_seasonality <- function(input_frame_s, input_frame_i,
                                        save_path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Figures",
                                        country = "de") {
  
  set_date_time()
  
  input_frame_s %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  input_frame_i %<>%
    filter(date < "2013-01-01") %>%
    group_by(date) %>%
    summarise(price = mean(price, na.rm = TRUE)) %>%
    ungroup
  
  trend_seas_fit_s <- seasonal_filter(input_frame_s)
  deseason_data_frame_s <- input_frame_s %>% 
    transmute(date,
              trend = 1:n(),
              dum_week = ifelse(format(date, "%a") == "Sat" | 
                                  format(date, "%a") == "Sun", 0, 1),
              price = price -
                (trend_seas_fit_s[1] + trend_seas_fit_s[2] * trend +
                   trend_seas_fit_s[3]  * sin((trend + trend_seas_fit_s[4])
                                              * 2 * pi / 365) +
                   trend_seas_fit_s[5] * sin((trend + trend_seas_fit_s[6])
                                             * 4 * pi / 365) +
                   trend_seas_fit_s[7] * dum_week),
              trend_seas = (trend_seas_fit_s[1] + trend_seas_fit_s[2] * trend +
                              trend_seas_fit_s[3]  * sin((trend + trend_seas_fit_s[4])
                                                         * 2 * pi / 365) +
                              trend_seas_fit_s[5] * sin((trend + trend_seas_fit_s[6])
                                                        * 4 * pi / 365) +
                              trend_seas_fit_s[7] * dum_week)) %>%
    select(-trend)
  
  trend_seas_fit_i <- seasonal_filter(input_frame_i)
  deseason_data_frame_i <- input_frame_i %>% 
    transmute(date,
              trend = 1:n(),
              dum_week = ifelse(format(date, "%a") == "Sat" | 
                                  format(date, "%a") == "Sun", 0, 1),
              price = price -
                (trend_seas_fit_i[1] + trend_seas_fit_i[2] * trend +
                   trend_seas_fit_i[3]  * sin((trend + trend_seas_fit_i[4])
                                              * 2 * pi / 365) +
                   trend_seas_fit_i[5] * sin((trend + trend_seas_fit_i[6])
                                             * 4 * pi / 365) +
                   trend_seas_fit_i[7] * dum_week),
              trend_seas = (trend_seas_fit_i[1] + trend_seas_fit_i[2] * trend +
                              trend_seas_fit_i[3]  * sin((trend + trend_seas_fit_i[4])
                                                         * 2 * pi / 365) +
                              trend_seas_fit_i[5] * sin((trend + trend_seas_fit_i[6])
                                                        * 4 * pi / 365) +
                              trend_seas_fit_i[7] * dum_week)) %>%
    select(-trend)
  
  plots <- list(
    p1 = ggplot(deseason_data_frame_s,
                aes(x = date, y = trend_seas)) +
      geom_line(color = "#003366") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Day-ahead seasonality") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(deseason_data_frame_i,
                aes(x = date, y = trend_seas)) +
      geom_line(color = "#003366") +
      xlab("Time") +
      ylab("Price, EUR/MWh") +
      ggtitle("Intraday seasonality") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank())
  )
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = paste0(country, "_escribano_season.eps"),
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_multi_line_plot <- function(input_frame_de, input_frame_fr, xlabel, ylabel, input, file_name = NULL,
                           save_path = NULL, do_print = TRUE) {
  plots <- list(
    p1 = ggplot(input_frame_de, aes_string(x = "date", y = input)) +
    geom_line(color = "#003366", size = 0.2) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle("Germany") +
    theme(axis.line = element_line(colour = "#E0E0DF"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(colour = "#656560"),
          axis.title.y = element_text(colour = "#656560"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E0E0DF"),
          panel.grid.minor.y = element_line(colour = "#E0E0DF"),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank()),
    p2 = ggplot(input_frame_fr, aes_string(x = "date", y = input)) +
      geom_line(color = "#003366", size = 0.2) +
      xlab(xlabel) +
      ylab(ylabel) +
      ggtitle("France") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
    
    p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_density <- function(input_frame_de, input_frame_fr, xlabel, ylabel, input, file_name = NULL,
                         save_path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Figures",
                         do_print = TRUE){
  plots <- list(
    p1 = ggplot(input_frame_de, aes_string(x = input)) +
    geom_density(color = "#003366", size = 0.2) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle("Germany") +
    theme(axis.line = element_line(colour = "#E0E0DF"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(colour = "#656560"),
          axis.title.y = element_text(colour = "#656560"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E0E0DF"),
          panel.grid.minor.y = element_line(colour = "#E0E0DF"),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank()),
    p2 = ggplot(input_frame_fr, aes_string(x = input)) +
      geom_density(color = "#003366", size = 0.2) +
      xlab(xlabel) +
      ylab(ylabel) +
      ggtitle("France") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_acf_and_per <- function(input_frame, lags, input, file_name = NULL,
                             save_path = "C:/Users/akl/Dropbox/Economics/Final_Thesis/Thesis/Figures",
                             do_print = TRUE) {
  
  stats_acf <- input_frame %>% `[`(, input) %>%
    acf(lag.max = lags, plot = FALSE) %>%
    with(data.frame(lag, acf))
  
  stats_pacf <- input_frame %>% `[`(, input) %>%
    pacf(lag.max = lags, plot = FALSE) %>%
    with(data.frame(lag, acf))
  
  acf_sig_level <- qnorm((1 + 0.95) / 2) / 
    sqrt(sum(!is.na(input_frame$price)))
  
  period <- spec.pgram(input_frame %>% `[`(, input),
                       taper = 0,
                       detrend = FALSE,
                       demean = FALSE,
                       plot = TRUE) %>%
    with(data.frame(spec = spec, freq = freq))
  
  plots <- list(
    p1 = ggplot(stats_acf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_hline(yintercept = -acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_segment(aes(xend = lag, yend = 0)) +
      ylim(0, 1) +
      xlab("Lag number") +
      ylab("Autocorrelation") +
      ggtitle("Autocorrelation function") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(stats_pacf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_hline(yintercept = -acf_sig_level, linetype = "dashed",
                 color = "#003366") +
      geom_segment(aes(xend = lag, yend = 0)) +
      ylim(0, 1) +
      xlab("Lag number") +
      ylab("Autocorrelation") +
      ggtitle("Partial autocorrelation function") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p3 = ggplot(period, aes(x = freq, y = spec)) +
      geom_line(color = "#003366") +
      scale_y_log10() +
      xlab("Frequency") +
      ylab("Spectrum") +
      ggtitle("Periodogram") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, plots$p3, nrow = 3)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.6,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_multi_line_plot_de <- function(input_frame_de, file_name = NULL,
                                 save_path = NULL, do_print = TRUE) {
  plots <- list(
    p1 = ggplot(input_frame_de, aes_string(x = "date", y = "cons_forecast")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Consumption forecast") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_de, aes_string(x = "date", y = "wind_forecast")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Wind production forecast") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p3 = ggplot(input_frame_de, aes_string(x = "date", y = "solar_forecast")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Solar production forecast") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p4 = ggplot(input_frame_de, aes_string(x = "date", y = "residual_load")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Residual load") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, plots$p3, plots$p4, nrow = 4)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.8,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_multi_line_plot_fr <- function(input_frame_de, file_name = NULL,
                                    save_path = NULL, do_print = TRUE) {
  plots <- list(
    p1 = ggplot(input_frame_de, aes_string(x = "date", y = "cons_forecast")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Consumption forecast") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_de, aes_string(x = "date", y = "wind_forecast")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Wind production forecast") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p3 = ggplot(input_frame_de, aes_string(x = "date", y = "residual_load")) +
      geom_line(color = "#003366", size = 0.2) +
      xlab("Time") +
      ylab("MW/h") +
      ggtitle("Residual load") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, plots$p3, nrow = 3)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.6,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_scatter <- function(input_frame_s, input_frame_i, input_frame_exp, country = "de",
                                    save_path = NULL, do_print = TRUE) {
  
  input_frame_s %<>% left_join(input_frame_exp, by = "date")
  input_frame_i %<>% left_join(input_frame_exp, by = "date")
  
  plots <- list(
    p1 = ggplot(input_frame_s, aes(x = residual_load, y = price)) +
      geom_point(color = "#003366") +
      xlab("Residual load, MWh") +
      ylab("Price, Euro per MWh") +
      ggtitle("Day ahead auction price") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()),
    p2 = ggplot(input_frame_i, aes(x = residual_load, y = price)) +
      geom_point(color = "#003366") +
      xlab("Residual load, MWh") +
      ylab("Price, Euro per MWh") +
      ggtitle("Intraday VWAP") +
      theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank()))
  
  p <- arrangeGrob(plots$p1, plots$p2, nrow = 2)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = paste0(country, "_scatter.eps"),
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_profit <- function(input_frame, market = "base", save_path = NULL,
                        do_print = TRUE) {
  
  colourCount = length(unique(input_frame$type))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  p <- input_frame %>%
    ggplot(aes(x = date, y = agg_prof, color = type)) %>%
    + geom_line() %>%
    + scale_color_manual(values = getPalette(colourCount)) %>% 
    + xlab("Time") %>% 
    + ylab("Profit in Euro") %>% 
    + ggtitle("Intraday VWAP") %>% 
    + theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E0DF"),
            panel.grid.minor.y = element_line(colour = "#E0E0DF"),
            panel.background = element_blank(),
            legend.position = "right",
            legend.key = element_blank())
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = paste0(market, "_profit_plot_hour_10.eps"),
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.8,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
draw_cap <- function(input_frame, save_path = NULL,
                     do_print = TRUE) {

  p <- input_frame %>%
    ggplot(aes(x = country, y = value, fill = Type)) %>%
    + geom_bar(stat="identity") %>%
    + scale_fill_brewer(palette = "Blues") %>% 
    + xlab("Country") %>% 
    + ylab("Percentage of type in generation mix") %>% 
    + theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_blank(),
            legend.position = "right",
            legend.key = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_text(size=12, face="bold"))
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = "generation_plot.eps",
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.4,
           units = "cm",
           dpi = 1000)
  }
}

#' @export
plot_forecast <- function(input_frame, filename, save_path, do_print = TRUE) {
  
  tmp <- input_frame %>% na.omit
  
  p <- input_frame %>%
    ggplot(aes(x = date, y = forecast, color = `Forecast type`)) %>%
    + geom_line(data = tmp, aes(x = date, y = price), color = "#cccccc", size = 0.2) %>% 
    + geom_line(size = 0.2) %>%
    + scale_color_manual(values = c("#003366", "#336600", "#990000")) %>% 
    + facet_wrap(~ market_con, nrow = 4) %>%
    + xlab("Date") %>%
    + ylab("Price, Euros per MWh") %>%
    + theme(axis.line = element_line(colour = "#E0E0DF"),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "#656560"),
            axis.title.y = element_text(colour = "#656560"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_text(size=12, face="bold"))
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = sprintf("forecast_plot_%s.eps", filename),
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 29.7 * 0.8,
           units = "cm",
           dpi = 1000)
  }
}