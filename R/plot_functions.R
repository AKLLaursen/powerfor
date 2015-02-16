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
    ylab(ylabel)
  
  # Print plot
  if (do_print == TRUE) print(p)
  
  # Possibly save graphs
  if (!is.null(save_path) && !is.null(file_name)) {
    ggsave(filename = file_name,
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 21 * 0.5,
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
           height = 21 * 0.5,
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
           height = 21 * 0.5,
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
      ggtitle("EEX day-ahead price") +
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
      ggtitle("EEX intraday VWAP") +
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
           height = 21,
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
      ggtitle("EEX day-ahead price") +
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
      ggtitle("EEX intraday VWAP") +
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
           height = 21 * 0.75,
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
      ggtitle("EEX day-ahead price") +
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
      ggtitle("EEX intraday VWAP") +
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
           height = 21 * 0.75,
           units = "cm",
           dpi = 1000)
  }
}