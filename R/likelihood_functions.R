#' Model estimation for Escribano2012 model a
#' 
#' @param theta An atomic vector of coefficients
#' @param price An atomic vector with the price series
#'
log_lik_model_a <- function(input_frame) {
  
#   set_data_time()
#   
#   data_frame <- input_frame %>%
#     filter(hour == 1)
#   
#   trend_seas_fit <- long_run_trend_season(data_frame)
#   de_lrts_data_frame <- data_frame %>% 
#     transmute(date,
#               trend = 1:n(),
#               ewma = ewma(price),
#               price = price - (trend_seas_fit[1] * 
#                                  sin(2 * pi * (trend / 365 + trend_seas_fit[2])) -
#                                  trend_seas_fit[3] + trend_seas_fit[4] * ewma))
#   
#   short_seas_fit <- short_run_season(de_lrts_data_frame)
#   deseason_data_frame <- data.frame(date = de_lrts_data_frame$date,
#                                     price = short_seas_fit$residuals)
#   
#   idx <- which(deseason_data_frame$price > mean(deseason_data_frame$price) +
#                  4 * sd(deseason_data_frame$price) | 
#                  deseason_data_frame$price < mean(deseason_data_frame$price) -
#                  4 * sd(deseason_data_frame$price))
#   
#   data_frame$price[idx] <- NA
#   
#   data_frame %<>% mutate(price = na_filter(price) %>% round(2))
#   
#   # Construct week dummies
#   week_dum <- data_frame %>%
#     transmute(week_day = date %>% as.Date %>% format("%a") %>% as.factor) %>%
#     model.matrix(~ week_day - 1,
#                  data = .) %>%
#     extract(, -2)
#   
#   # Construct weekend dummies
#   week_dum <- data_frame %>%
#     transmute(weekend = date %>%
#                 as.Date %>%
#                 format("%a")) %>%
#     mutate(weekend = ifelse(weekend == "Sat" | weekend == "Sun", 1, 0)) %>%
#     as.matrix
#   
#   theta <- rep(0.1, 20)
#   theta[19:20] <- 5
#   
#   test <- loglik_a_sim_sum(theta, data_frame$price, week_dum)
#   
#   b <- rep(0, 5)
#   A <- matrix(0, 5, length(theta))
#   A[1, 15] <- 1
#   A[2, 16] <- 1
#   A[3, 17] <- 1
#   A[4, 16:17] <- -1
#   A[5, 16:17] <- 1
#   b[4] <- 1
#   
#   
#   max <- maxLik(loglik_a_sim, start = theta, price = data_frame$price, week_dum = week_dum,
#          method = "BHHH", iterlim = 100000, print.level = 2, steptol = 1e-20)
#   
#   max1 <- maxLik(loglik_a_sim_sum, start = theta, price = data_frame$price, week_dum = week_dum,
#                  constraints = list(ineqA = A,
#                                     ineqB = b),
#                  method = "BFGS", iterlim = 100000, print.level = 2, tol = 1e-20)
#   
#   max2 <- maxLik(loglik_a_sim_sum, start = theta, price = data_frame$price, week_dum = week_dum,
#                  constraints = list(ineqA = A,
#                                     ineqB = b),
#                  method = "NM", iterlim = 100000, print.level = 2, tol = 1e-20)
#   
#   max3 <- maxLik(loglik_a_sim_sum, start = theta, price = data_frame$price, week_dum = week_dum,
#                  constraints = list(ineqA = A,
#                                     ineqB = b),
#                  method = "CG", iterlim = 100000, print.level = 2, tol = 1e-20)
#   
#   max4 <- maxLik(loglik_a_sim_sum, start = theta, price = data_frame$price, week_dum = week_dum,
#                  constraints = list(ineqA = A,
#                                     ineqB = b),
#                  method = "SANN", iterlim = 100000, print.level = 2, tol = 1e-20)
#   
#   lower <- rep(-Inf, 20)
#   lower[15:17] <- 0
#   
#   
#   max5 <- nlminb(theta,
#                  loglik_a_sim_sum_neg,
#                  control = list(eval.max = 10000,
#                                 iter.max = 10000,
#                                 trace = 2),
#                  lower = lower,
#                  price = data_frame$price,
#                  week_dum = week_dum)
#   
#   max5$par[16:17] <- 0.1
#   
#   max <- maxLik(loglik_a_sim_sum, start = max5$par, price = data_frame$price, week_dum = week_dum,
#                  constraints = list(ineqA = A,
#                                     ineqB = b),
#                  method = "NM", iterlim = 100000, print.level = 2, tol = 1e-20) %>% summary
#   
# eval_f0 <- function(theta, data_frame$price, week_dum) {
#     b <- rep(0, 5)
#     A <- matrix(0, 5, length(theta))
#     A[1, 15] <- 1
#     A[2, 16] <- 1
#     A[3, 17] <- 1
#     A[4, 16:17] <- -1
#     A[5, 16:17] <- 1
#     b[4] <- 1
#     y <- A * theta - b
#     return(y)
#   }
#   
#   nloptr::nloptr(theta, loglik_a_sim_sum_neg,
#                  eval_f = eval_f0,
#                  opts = list(algorithm = "LN_BOBYQA",
#                              print_level = 2),
#                  price = data_frame$price,
#                  week_dum = week_dum)
#   
#   return(short_seas_fit)
}