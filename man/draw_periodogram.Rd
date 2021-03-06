% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{draw_periodogram}
\alias{draw_periodogram}
\title{Function plotting the periodogram for a univariate time series}
\usage{
draw_periodogram(input_frame, input, log = TRUE, file_name = NULL,
  save_path = NULL, do_print = TRUE)
}
\arguments{
\item{input_frame}{A dataframe with a series named price}

\item{input}{A string with input variable name}

\item{log}{A bolean indicating if y-axis shcould be logarithmic}

\item{save_path}{A string with path to save file}

\item{do_print}{A Bolean indicating if plot should be printed}

\item{file}{name A string indicating name of output}
}
\description{
Function plotting the periodogram for a univariate time series
}

