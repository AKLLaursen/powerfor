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